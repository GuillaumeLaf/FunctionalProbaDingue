namespace Model

module MonadicGraph = 
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
    
    type State<'T> = State of parameters:'T[] * variables:'T[]

    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Multiplication, N1, N2)

    let fixedParameterM idx = 
        let innerFunc (State(p,v)) = p.[idx], (State(p,v))
        Monad.M innerFunc

    let fixedVariableM idx = 
        let innerFunc (State(p,v)) = v.[idx], (State(p,v))
        Monad.M innerFunc

    let fold nodeF leafV sk = 
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


    let ma1:Skeleton<float> = Leaf(Parameter(0)) .*. Leaf(Variable(0)) .+. Leaf(Constant(1.0))