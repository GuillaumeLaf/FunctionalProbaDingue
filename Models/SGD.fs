namespace Models

open Monads

module SGD = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    type Optimizer = 
        | Classic of learningRate:float
        | Momentum of momentumRate:float * momentumValue:float [] * learningRate:float
        | RMSProp of momentumRate:float * momentumValue:float [] * learningRate:float
        
    let limitParams x =
        match x with
        | a when x < -1.0 -> -1.0
        | a when x > 1.0 -> 1.0
        | _ -> x

    let currentErrorGradientM skeleton idx = 
        Monad.state {
            do! GraphTS.runTimeSeriesM (TimeSeries.Univariate.setCurrentIndexM idx)
            let! currentError = GraphTS.runTimeSeriesM (TimeSeries.Univariate.currentInnovationM ())
            let currentError = currentError |> Option.defaultValue 0.0
            let! currentGradient = GraphTS.runGraphM (Graph.skeletonGradientM skeleton)
            return Array.map (fun g -> g * currentError) currentGradient
        }

    let aggregateCurrentErrorGradientM nParameters skeleton indices = 
        Monad.state {
            let gradient = Array.zeroCreate nParameters
            let! currentGradients = Array.map (fun idx -> currentErrorGradientM skeleton idx) indices |> Monad.mapM
            let aggregateGradient = Array.foldBack (fun x s -> UtilitiesSIMD.ArraySIMD.add x s) currentGradients gradient
            return UtilitiesSIMD.ArraySIMD.multScalar (1.0/float nParameters) aggregateGradient
        }

    let updatedOptimizerAndParameters parameterValues gradient = function
        | Classic(learningRate) as x -> x, Array.map2 (fun p g -> p - learningRate * g) parameterValues gradient
        | Momentum(momRate,momValue,learnRate) -> let newMomentum = Array.map2 (fun mm g-> momRate*mm - learnRate*g) momValue gradient
                                                  Momentum(momRate,newMomentum,learnRate), UtilitiesSIMD.ArraySIMD.add parameterValues newMomentum
        | RMSProp(momRate,momValue,learnRate) -> let newMomentum = Array.map2 (fun mm g -> momRate*mm + (1.0-momRate)*g*g) momValue gradient
                                                 RMSProp(momRate,newMomentum,learnRate), Array.map3 (fun p mm g -> p - g*(learnRate/(sqrt(mm)+0.0001)))
                                                                                                    parameterValues momValue gradient

    // Updating of the parameters must be made after running the model.
    let updateParametersM optimizer skeleton indices = 
        Monad.state {
            let! parameterValues = GraphTS.runGraphM Graph.parametersM
            let! gradient = aggregateCurrentErrorGradientM (parameterValues.Length) skeleton indices
            let newOptimizer,newParameters = updatedOptimizerAndParameters parameterValues gradient optimizer
            let newParameters = newParameters |> Array.map (fun x -> limitParams x)
            do! GraphTS.runGraphM (Graph.setParametersM newParameters)
            return newOptimizer
        }
            
    let fit model optimizer epochs array = 
        let model = (ErrorModel(model))
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = Graph.defaultStateForFitting model
        let defaultSk = Graph.defaultSkeletonForFitting model
        let errorSkM = model |> Fitting |> Graph.modelM
        let updateVarM = GraphTS.updateForFittingM model

        let fittingM opt indices = 
            Monad.state {
                do! GraphTS.SDGfitM updateVarM errorSkM indices // compute the error
                return! updateParametersM opt defaultSk indices
            }
                         
        // function that will fold on the indices. 
        let folder opt states = Array.fold (fun s idx -> let newOpt,newStates = s
                                                         let nxtOpt,nxtStates = newStates |> Monad.run (fittingM newOpt idx) 
                                                         (nxtOpt,nxtStates)) (opt,states)                              
        
        let printCurrent states = 
            let (Graph.State(p,_,_),TimeSeries.Univariate.State(_,_,errors)) = states
            let errors = errors |> Array.map (fun x -> x |> Option.defaultValue 0.0)
            errors |> Array.mapFold (fun s x -> (),x*x+s) 0.0 |> snd, p

        let rec loop idxArray epochs states = 
            match epochs with
            | 1 -> printfn "%A" (printCurrent states); folder optimizer states (idxArray |> Array.chunkBySize 1)
            | _ -> printfn "%A" (printCurrent states); loop (Utilities.shuffle idxArray) (epochs-1) (folder optimizer states (idxArray |> Array.chunkBySize 1) |> snd)
        loop (Array.init array.Length id) epochs (initStateG,initStateTS) |> snd |> fst
