namespace Models

open MathNet.Numerics.Distributions
open Monads

module MonadicGraph = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    type Op = 
        | Addition
        | Multiplication

    type Input<'T> = 
        | Parameter of idx:int 
        | Variable of idx:int
        | Innovation of idx:int
        | Constant of value:'T

    type Skeleton<'T> = 
        | Leaf of Input<'T> 
        | Node of Op * Skeleton<'T> * Skeleton<'T>

    type SkeletonType = 
        | Sampling
        | Fitting
    
    type State<'T> = State of parameters:'T[] * variables:'T[] * innovations:'T[]

    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Multiplication, N1, N2)

    let defaultState = function
        | AR(order) | MA(order) -> State(Array.zeroCreate order, Array.zeroCreate order,[|0.0|])
        //| SETAR(order,delay) -> State(Array.zeroCreate (2*order+1), Array.zeroCreate (order+1))

    let getParameterM idx = 
        let innerFunc (State(p,v,i)) = p.[idx], (State(p,v,i))
        Monad.M innerFunc

    let getVariableM idx = 
        let innerFunc (State(p,v,i)) = v.[idx], (State(p,v,i))
        Monad.M innerFunc

    let getInnovationM idx = 
        let innerFunc (State(p,v,i)) = i.[idx], (State(p,v,i))
        Monad.M innerFunc

    let inactiveInnovationM idx = Monad.rets 0.0

    let setParameterM idx x = 
        let innerFunc (State(p,v,i)) = 
            p.[idx] <- x
            x, (State(p,v,i))
        Monad.M innerFunc

    let setVariableM idx x = 
        let innerFunc (State(p,v,i)) = 
            v.[idx] <- x
            x, (State(p,v,i))
        Monad.M innerFunc

    let setInnovationM idx x = 
        let innerFunc (State(p,v,i)) = 
            i.[idx] <- x
            x, (State(p,v,i))
        Monad.M innerFunc

    let setParametersM array = 
        array |> Array.indexed
              |> Array.toList
              |> Monad.traverse (fun (i,x) -> setParameterM i x)
              |> Monad.map (Array.ofList)

    let setVariablesM array = 
        array |> Array.indexed
              |> Array.toList
              |> Monad.traverse (fun (i,x) -> setVariableM i x)
              |> Monad.map (Array.ofList)

    let setInnovationsM array = 
        array |> Array.indexed
              |> Array.toList
              |> Monad.traverse (fun (i,x) -> setInnovationM i x)
              |> Monad.map (Array.ofList)

    let inline fold nodeF leafV sk = 
        let rec loop n k = 
            match n with
                | Node(op,left,right) -> nodeF op (loop left) (loop right) n k
                | Leaf(input) -> leafV input n k
        loop sk id

    let inline skeletonM parameterM variableM innovationM skeleton = 
        fold (fun op kl kr _ k -> match op with
                                    | Addition -> kl (fun lacc -> kr (fun racc -> (Monad.add lacc racc) |> k)) 
                                    | Multiplication -> kl (fun lacc -> kr (fun racc ->  (Monad.mult lacc racc) |> k))
                                    )
             (fun input _ k -> match input with
                                | Parameter(idx) -> parameterM idx |> k
                                | Variable(idx) -> variableM idx |> k
                                | Innovation(idx) -> innovationM idx |> k  
                                | Constant(value) -> Monad.rets value |> k)
             skeleton 

    let defaultSkeleton = function
        | AR(order) | MA(order) -> Array.zeroCreate order |> Array.mapi (fun i _ -> Leaf(Parameter(i)) .*. Leaf(Variable(i)))
                                                          |> Array.reduce (.+.)
                                                          |> (.+.) (Leaf(Innovation(0)))

    let activateSkeletonM sk = function
        | Sampling -> Monad.modify (fun (State(p,v,i)) -> State(p,v,[|Normal.Sample(0.0,1.0)|]))
                        >>= (fun _ -> skeletonM getParameterM getVariableM getInnovationM sk)
        | Fitting -> skeletonM getParameterM getVariableM inactiveInnovationM sk

    let modelM = defaultSkeleton >> activateSkeletonM

