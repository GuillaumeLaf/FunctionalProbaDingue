namespace Models

open Monads

module SGD = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    type Optimizer = 
        | Classic of learnRate:float
        | Momentum of momRate:float * momValue:float [] * learnRate:float
        | RMSProp of momRate:float * momValue:float [] * learnRate:float
        | ADAM of momRate1:float * momRate2:float * momValue1:float[] * momValue2:float[] * learnRate:float
        
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
            return UtilitiesSIMD.ArraySIMD.multScalar (1.0/float indices.Length) aggregateGradient
        }

    let updatedOptimizerAndParameters parameterValues gradient = function
        | Classic(learningRate) as x -> x, Array.map2 (fun p g -> p - learningRate * g |> limitParams) parameterValues gradient
        | Momentum(momRate,momValue,learnRate) -> let newMomentum = Array.map2 (fun mm g-> momRate*mm - learnRate*g) momValue gradient
                                                  Momentum(momRate,newMomentum,learnRate), UtilitiesSIMD.ArraySIMD.add parameterValues newMomentum
        | RMSProp(momRate,momValue,learnRate) -> let newMomentum = Array.map2 (fun mm g -> momRate*mm + (1.0-momRate)*g*g) momValue gradient
                                                 RMSProp(momRate,newMomentum,learnRate), Array.map3 (fun p mm g -> p - g*(learnRate/(sqrt(mm)+1.0)) |> limitParams)
                                                                                                    parameterValues momValue gradient
        | ADAM(momRate1,momRate2,momValue1,momValue2,learnRate) -> let newMom1 = Array.map2 (fun m1 g -> momRate1*m1 + (1.0-momRate1)*g) momValue1 gradient
                                                                   let newMom2 = Array.map2 (fun m2 g -> momRate2*m2 + (1.0-momRate2)*g*g) momValue2 gradient
                                                                   ADAM(momRate1,momRate2,momValue1,momValue2,learnRate),
                                                                        Array.map3 (fun nm1 nm2 p -> p - learnRate*(nm1/(1.0-momRate1))/(sqrt(nm2/(1.0-momRate2))+0.001) |> limitParams) newMom1 newMom2 parameterValues

    // Updating of the parameters must be made after running the model.
    let updateParametersM optimizer skeleton indices = 
        Monad.state {
            let! parameterValues = GraphTS.runGraphM Graph.parametersM
            let! gradient = aggregateCurrentErrorGradientM (parameterValues.Length) skeleton indices
            let newOptimizer,newParameters = updatedOptimizerAndParameters parameterValues gradient optimizer
            //let newParameters = newParameters |> Array.map (fun x -> limitParams x)
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
            | 1 -> printfn "%A" (printCurrent states); folder optimizer states (idxArray |> Array.chunkBySize 12)
            | _ -> printfn "%A" (printCurrent states); loop (Utilities.shuffle idxArray) (epochs-1) (folder optimizer states (idxArray |> Array.chunkBySize 12) |> snd)
        loop (Array.init array.Length id) epochs (initStateG,initStateTS) |> snd |> fst
