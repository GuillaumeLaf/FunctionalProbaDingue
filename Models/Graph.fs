namespace Models

open MathNet.Numerics.Distributions
open Monads

module Graph = 
    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    type State<'T> = State of parameters:'T[] * variables:'T[] * innovations:'T[]

    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Multiplication, N1, N2)

    let rec convertModelToParameters = function
        | AR(order) -> ARp(Array.zeroCreate order)
        | MA(order) -> MAp(Array.zeroCreate order)
        | STAR(order,_,_,innerModel) -> STARp(Array.zeroCreate order, Array.zeroCreate order, 0.0, 1.0, convertModelToParameters innerModel)

    let rec defaultStateForSampling = function
        | ARp(coeffs) | MAp(coeffs) -> State(coeffs, Array.zeroCreate coeffs.Length, [|0.0|])
        | STARp(coeffs1,coeffs2,_,_,innerModelp) -> let (State(innerCoeffs,_,_)) = defaultStateForSampling innerModelp
                                                    State(Array.concat [|coeffs1;coeffs2;innerCoeffs|],Array.zeroCreate (coeffs1.Length+innerCoeffs.Length),[|0.0|])

    let defaultStateForFitting = convertModelToParameters >> defaultStateForSampling

    let defaultState = function
        | Sampling(mparameters) -> defaultStateForSampling mparameters
        | Fitting(model) -> defaultStateForFitting model

    let stateM = 
        let innerFunc state = state,state
        Monad.M innerFunc

    let parametersM = 
        let innerFunc (State(p,v,i)) = p, (State(p,v,i))
        Monad.M innerFunc

    let variablesM = 
        let innerFunc (State(p,v,i)) = v, (State(p,v,i))
        Monad.M innerFunc

    let innovationsM = 
        let innerFunc (State(p,v,i)) = i, (State(p,v,i))
        Monad.M innerFunc

    let getParameterM idx = Array.get <!> parametersM <*> (Monad.rets idx)
    let getVariableM idx = Array.get <!> variablesM <*> (Monad.rets idx)
    let getInnovationM idx = Array.get <!> innovationsM <*> (Monad.rets idx)

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

    let mapM mArray = 
        let innerFunc state = 
            mArray |> Array.map (fun m -> Monad.run m state |> fst), state
        Monad.M innerFunc

    let inline skeletonM parameterM variableM innovationM skeleton = 
        SkeletonTree.fold (fun op nk _ k -> match op with
                                                | Apply(f) -> nk (fun nacc -> Monad.map f nacc |> k))
                          (fun op kl kr _ k -> match op with
                                                | Addition -> kl (fun lacc -> kr (fun racc -> (Monad.add lacc racc) |> k)) 
                                                | Multiplication -> kl (fun lacc -> kr (fun racc ->  (Monad.mult lacc racc) |> k))
                                                | Substraction -> kl (fun lacc -> kr (fun racc ->  (Monad.sub lacc racc) |> k))
                                                )
                          (fun input _ k -> match input with
                                                | Parameter(idx) -> parameterM idx |> k
                                                | Variable(idx) -> variableM idx |> k
                                                | Innovation(idx) -> innovationM idx |> k  
                                                | Constant(value) -> Monad.rets value |> k)
                          skeleton 

    let skeletonGradientForParameterM idx skeleton = 
        let skM = skeletonM getParameterM getVariableM getInnovationM skeleton
        let getParameterShiftedM idx x = if idx = x then Monad.add (getParameterM idx) (Monad.rets 0.00005) else getParameterM x
        let skShiftParameterM idx = skeletonM (getParameterShiftedM idx) getVariableM getInnovationM skeleton
        Monad.div (Monad.sub (skShiftParameterM idx) skM) (Monad.rets 0.00005)

    let skeletonGradientM skeleton = 
        Array.mapi <!> (Monad.rets (fun i _ -> skeletonGradientForParameterM i skeleton)) 
                   <*> parametersM
                   >>= (fun arrayM -> mapM arrayM)
                   
    let rec defaultSkeletonForSampling = function
        | ARp(coeffs) | MAp(coeffs) -> Nodes.linearCombinaisons coeffs.Length .+. (Leaf(Innovation(0)))
        | STARp(coeffs1,coeffs2,loc,scale,innerModelp) -> let ARs = defaultSkeletonForSampling (ARp(coeffs1))
                                                          let expTerm x = ((-x+loc)/scale) |> exp
                                                          let logisticFunc x = 1.0 / (1.0 + expTerm x) 
                                                          let mixingNode = Node1(Apply(logisticFunc),defaultSkeletonForSampling innerModelp |> SkeletonTree.deactivateInnovations)
                                                          //let mixingNode = Node1(Apply(logisticFunc),Nodes.linearCombinaisons coeffs1.Length)
                                                          (ARs,ARs) ||> Nodes.mixture id (fun _ -> 0) (fun _ -> 0) mixingNode

    let defaultSkeletonForFitting = convertModelToParameters >> defaultSkeletonForSampling

    let activateSkeletonForSamplingM sk = Monad.modify (fun (State(p,v,i)) -> State(p,v,[|Normal.Sample(0.0,1.0)|]))
                                            >>= (fun _ -> skeletonM getParameterM getVariableM getInnovationM sk)

    let activateSkeletonForFittingM sk = skeletonM getParameterM getVariableM inactiveInnovationM sk

    let modelM = function
        | Sampling(mparameters) -> (defaultSkeletonForSampling >> activateSkeletonForSamplingM) mparameters
        | Fitting(model) -> (defaultSkeletonForFitting >> activateSkeletonForFittingM) model

