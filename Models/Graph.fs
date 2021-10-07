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
    let inline ( .-. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Substraction, N1, N2)

    let rec convertModelToParameters = function
        | AR(order) -> ARp(Array.zeroCreate order)
        | MA(order) -> MAp(Array.zeroCreate order)
        | STAR(order,_,_,innerModel) -> STARp(Array.zeroCreate order, Array.zeroCreate order, 0.0, 1.0, convertModelToParameters innerModel)
        | ErrorModel(innerModel) -> ErrorModelp(convertModelToParameters innerModel)

    let rec defaultStateForSampling = function
        | ARp(coeffs) | MAp(coeffs) -> State(coeffs, Array.zeroCreate coeffs.Length, [|0.0|])
        | STARp(coeffs1,coeffs2,_,_,innerModelp) -> let (State(innerCoeffs,_,_)) = defaultStateForSampling innerModelp
                                                    State(Array.concat [|coeffs1;coeffs2;innerCoeffs|],Array.zeroCreate (coeffs1.Length+innerCoeffs.Length),[|0.0|])
        | ErrorModelp(innerModelp) -> let (State(innerCoeffs,innerVar,innerInnov)) = defaultStateForSampling innerModelp
                                      (State(innerCoeffs,Array.concat [|[|0.0|];innerVar|],innerInnov))
    
    let defaultStateForFitting = convertModelToParameters >> defaultStateForSampling

    let defaultState = function
        | Sampling(mparameters) -> defaultStateForSampling mparameters
        | Fitting(model) -> defaultStateForFitting model

    let parametersM = Monad.M (fun (State(p,v,i)) -> p,(State(p,v,i)))
    let variablesM = Monad.M (fun (State(p,v,i)) -> v,(State(p,v,i)))
    let innovationsM = Monad.M (fun (State(p,v,i)) -> i,(State(p,v,i)))

    let getParameterM idx = Array.get <!> parametersM <*> (Monad.rets idx)
    let getVariableM idx = Array.get <!> variablesM <*> (Monad.rets idx)
    let getInnovationM idx = Array.get <!> innovationsM <*> (Monad.rets idx)

    let inactiveInnovationM idx = Monad.rets 0.0

    let setParameterM idx x = 
        let innerFunc (State(p,v,i)) = 
            p.[idx] <- x
            (), (State(p,v,i))
        Monad.M innerFunc

    let setVariableM idx x = 
        let innerFunc (State(p,v,i)) = 
            v.[idx] <- x
            (), (State(p,v,i))
        Monad.M innerFunc

    let setInnovationM idx x = 
        let innerFunc (State(p,v,i)) = 
            i.[idx] <- x
            (), (State(p,v,i))
        Monad.M innerFunc

    let setParametersM array = Array.mapi (fun i x -> setParameterM i x) array |> Monad.mapM >>= (fun _ -> Monad.rets ())
    let setVariablesM array = Array.mapi (fun i x -> setVariableM i x) array |> Monad.mapM >>= (fun _ -> Monad.rets ())
    let setInnovationsM array = Array.mapi (fun i x -> setInnovationM i x) array |> Monad.mapM >>= (fun _ -> Monad.rets ())

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

    let skeletonGradientForParameterM wantedIdx skeleton = 
        let getParameterShiftedM wantedIdx = function 
            | x when x=wantedIdx -> Monad.add (getParameterM wantedIdx) (Monad.rets 0.00005) 
            | x -> getParameterM x
        Monad.state {
            let! x = skeletonM getParameterM getVariableM getInnovationM skeleton
            let! x_dt = skeletonM (getParameterShiftedM wantedIdx) getVariableM getInnovationM skeleton
            return (x_dt - x)/0.00005
        }

    let skeletonGradientM skeleton = 
        Array.mapi <!> (Monad.rets (fun i _ -> skeletonGradientForParameterM i skeleton)) 
                   <*> parametersM
                   >>= (fun arrayM -> Monad.mapM arrayM)
                   
    let rec defaultSkeletonForSampling = function
        | ARp(coeffs) | MAp(coeffs) -> Nodes.linearCombinaisons coeffs.Length .+. (Leaf(Innovation(0)))
        | STARp(coeffs1,coeffs2,loc,scale,innerModelp) -> let ARs = defaultSkeletonForSampling (ARp(coeffs1))
                                                          let expTerm x = ((-x+loc)/scale) |> exp
                                                          let logisticFunc x = 1.0 / (1.0 + expTerm x) 
                                                          let mixingNode = Node1(Apply(logisticFunc),defaultSkeletonForSampling innerModelp |> SkeletonTree.deactivateInnovations)
                                                          //let mixingNode = Node1(Apply(logisticFunc),Nodes.linearCombinaisons coeffs1.Length)
                                                          (ARs,ARs) ||> Nodes.mixture id (fun _ -> 0) (fun _ -> 0) mixingNode
        | ErrorModelp(innerModelp) -> (defaultSkeletonForSampling innerModelp |> SkeletonTree.shift 0 1 0) .-. (Leaf(Variable(0)))

    let defaultSkeletonForFitting = convertModelToParameters >> defaultSkeletonForSampling

    let activateSkeletonForSamplingM sk = 
        Monad.state {
            let! (State(p,v,_)) = Monad.get
            do! Monad.put (State(p,v,[|Normal.Sample(0.0,1.0)|]))
            return! skeletonM getParameterM getVariableM getInnovationM sk
        }

    let activateSkeletonForFittingM sk = skeletonM getParameterM getVariableM inactiveInnovationM sk

    let modelM = function
        | Sampling(mparameters) -> (defaultSkeletonForSampling >> activateSkeletonForSamplingM) mparameters
        | Fitting(model) -> (defaultSkeletonForFitting >> activateSkeletonForFittingM) model

