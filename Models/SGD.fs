namespace Models

open Monads

module SGD = 
    // https://ruder.io/optimizing-gradient-descent/index.html#amsgrad

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    type Optimizer = 
        | Classic of learnRate:float
        | Momentum of momRate:float * learnRate:float
        | RMSProp of momRate:float * learnRate:float
        | Adam of momRateG:float * momRateG2:float * learnRate:float
        | AMSGrad of momRateG:float * momRateG2:float * learnRate:float

    type OptimizerParameters = 
        | ClassicP
        | MomentumP of momentumValue:float[]
        | RMSPropP of momentumValue:float[]
        | AdamP of momentumValueG:float[] * momentumValueG2:float[] * time:int
        | AMSGradP of momentumValueG:float[] * momentumValueG2:float[] * momentumMax:float[]
        
    let limitParams x =
        match x with
        | a when x < -1.0 -> -1.0
        | a when x > 1.0 -> 1.0
        | _ -> x

    let rec defaultArrayLengthForModel = function
        | AR(n) -> Array.zeroCreate n
        | MA(n) -> Array.zeroCreate n
        | STAR(n,_,_,innerModel) -> Array.concat [|Array.zeroCreate (2*n); defaultArrayLengthForModel innerModel|]
        | ErrorModel(innerModel) -> defaultArrayLengthForModel innerModel

    let defaultOptimizerParameters model = function
        | Classic(_) -> ClassicP
        | Momentum(_,_) -> MomentumP(defaultArrayLengthForModel model)
        | RMSProp(_,_) -> RMSPropP(defaultArrayLengthForModel model)
        | Adam(_,_,_) -> AdamP(defaultArrayLengthForModel model,defaultArrayLengthForModel model,1)
        | AMSGrad(_,_,_) -> AMSGradP(defaultArrayLengthForModel model,defaultArrayLengthForModel model,defaultArrayLengthForModel model)

    let currentErrorGradientM errorSk idx = 
        Monad.state {
            do! GraphTS.runTimeSeriesM (TimeSeries.Univariate.setCurrentIndexM idx)
            let! currentError = GraphTS.runTimeSeriesM (TimeSeries.Univariate.currentInnovationM ())
            let currentError = currentError |> Option.defaultValue 0.0
            let! currentGradient = GraphTS.runGraphM (Graph.skeletonGradientM errorSk)
            return Array.map (fun g -> 2.0 * g * currentError) currentGradient
        }

    // Compute the gradient for the given batch of indices
    let aggregateCurrentErrorGradientM nParameters errorSk indices = 
        Monad.state {
            let! currentGradients = Array.map (fun idx -> currentErrorGradientM errorSk idx) indices |> Monad.mapM
            return Array.foldBack (fun x s -> UtilitiesSIMD.ArraySIMD.multScalar (1.0/float indices.Length) x
                                                 |> UtilitiesSIMD.ArraySIMD.add s) currentGradients (Array.zeroCreate nParameters)
        }

    let updatedOptimizerAndParameters parameterValues gradient = function
        | Classic(learningRate),ClassicP -> ClassicP, Array.map2 (fun p g -> p - learningRate * g) parameterValues gradient
        | Momentum(momRate,learnRate),MomentumP(pastMomentum) -> let newMomentum = Array.map2 (fun mm g-> - momRate*mm - learnRate*g) pastMomentum gradient
                                                                 MomentumP(newMomentum), UtilitiesSIMD.ArraySIMD.add parameterValues newMomentum
        | RMSProp(momRate,learnRate),RMSPropP(pastMomentum) -> let newMomentum = Array.map2 (fun mm g -> momRate*mm + (1.0-momRate)*g*g) pastMomentum gradient
                                                               RMSPropP(newMomentum), Array.map3 (fun p mm g -> p - g*(learnRate/(sqrt(mm+1e-8))))
                                                                                                    parameterValues newMomentum gradient
        | Adam(momRateG,momRateG2,learnRate),AdamP(pastMomG,pastMomG2,time) -> let newMomG = Array.map2 (fun mmG g -> momRateG*mmG + (1.0-momRateG)*g) pastMomG gradient
                                                                               let newMomG2 = Array.map2 (fun mmG2 g -> momRateG2*mmG2 + (1.0-momRateG2)*g*g) pastMomG2 gradient
                                                                               let t = float time
                                                                               AdamP(newMomG,newMomG2,time+1), Array.map3 (fun p mmG mmG2 -> p - (mmG/(1.0-momRateG**t))*(learnRate/(sqrt(mmG2/(1.0-momRateG2**t))+1e-8)))
                                                                                                                          parameterValues newMomG newMomG2
        | AMSGrad(momRateG,momRateG2,learnRate),AMSGradP(pastMomG,pastMomG2,pastMomMax) -> let newMomG = Array.map2 (fun mmG g -> momRateG*mmG + (1.0-momRateG)*g) pastMomG gradient
                                                                                           let newMomG2 = Array.map2 (fun mmG2 g -> momRateG2*mmG2 + (1.0-momRateG2)*g*g) pastMomG2 gradient
                                                                                           let newMomMax = Array.map2 (fun mmMax mmG2 -> max mmMax mmG2) pastMomMax newMomG2
                                                                                           AMSGradP(newMomG,newMomG2,newMomMax), Array.map3 (fun p mmG mmMax -> p - mmG*learnRate / (sqrt(mmMax)+1e-8))
                                                                                                                                            parameterValues newMomG newMomMax
        | _ -> invalidArg "Optimizer" "Unknown optimizer or not right parameters."

    // Updating of the parameters must be made after running the model.
    let updateParametersM optimizer skGradient indices = 
        Monad.state {
            let! parameterValues = GraphTS.runGraphM Graph.parametersM
            let! gradient = aggregateCurrentErrorGradientM (parameterValues.Length) skGradient indices
            let newOptimizerHist,newParameters = updatedOptimizerAndParameters parameterValues gradient optimizer
            let newParameters = newParameters |> Array.map (fun x -> limitParams x)
            do! GraphTS.runGraphM (Graph.setParametersM newParameters)
            return newOptimizerHist
        }
            
    let fit model optimizer epochs array = 
        let model = (ErrorModel(model))
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = Graph.defaultStateForFitting model
        let defaultSk = Graph.defaultSkeletonForFitting model
        let errorSkM = model |> Fitting |> Graph.modelM
        let updateVarM = GraphTS.updateForFittingM model
        let defaultOptParams = defaultOptimizerParameters model optimizer
        let skeletonGradientM = Graph.skeletonGradientM defaultSk
        

        let fittingM optHist indices = 
            Monad.state {
                do! GraphTS.SDGfitM updateVarM errorSkM indices // compute the error
                return! updateParametersM (optimizer,optHist) defaultSk indices
            }
                         
        // function that will fold on the indices. 
        let folder optInitHistory states = Array.fold (fun s idx -> let newOptHist,newStates = s
                                                                    let nxtOpt,nxtStates = newStates |> Monad.run (fittingM newOptHist idx) 
                                                                    (nxtOpt,nxtStates)) (optInitHistory,states)                              
        let err = Array.zeroCreate epochs
        let printCurrent states epochs = 
            let (Graph.State(p,_,_),TimeSeries.Univariate.State(_,_,errors,_)) = states
            let errors = errors |> Array.map (fun x -> x |> Option.defaultValue 0.0)
            let errorSquared = errors |> Array.mapFold (fun s x -> (),x*x+s) 0.0 |> snd
            err.[epochs-1] <- errorSquared
            errorSquared, p

        let rec loop idxArray epochs optHist states = 
            match epochs with
            | 1 -> printfn "%A" (printCurrent states epochs); folder optHist states (idxArray |> Array.chunkBySize 64)
            | _ -> let newOptHist, newStates = folder optHist states (idxArray |> Array.chunkBySize 64)
                   printfn "%A" (printCurrent states epochs); 
                   loop (Utilities.shuffle idxArray) (epochs-1) newOptHist newStates
        loop (Array.init array.Length id) epochs defaultOptParams (initStateG,initStateTS) |> snd |> fst, Array.rev err
