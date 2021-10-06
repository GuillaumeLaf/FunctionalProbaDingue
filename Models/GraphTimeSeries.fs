namespace Models

open Monads

module GraphTS = 
    let (>>=) x f = Monad.bind f x
    let (<!>) = Monad.map
    let (<*>) = Monad.apply
    let (>=>) g f = Monad.compose g f

    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec defineUpdatesM modelName = 
        let rec updateSequenceTSM = function
            | ARp(coeffs) -> [| for i in 1..coeffs.Length do TimeSeries.Univariate.elementAtLagM i |]
            | MAp(coeffs) -> [| for i in 1..coeffs.Length do TimeSeries.Univariate.innovationAtLagM i |] 
            | STARp(coeffs1,coeffs2,_,_,innerModelp) -> Array.concat[|updateSequenceTSM (ARp(coeffs1)); updateSequenceTSM innerModelp|]       
        updateSequenceTSM modelName
        |> Monad.mapM
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))

    let updateForFittingM = Graph.convertModelToParameters >> defineUpdatesM

    let stateGraphM () = fst <!> Monad.get
    let stateTimeSeriesM () = snd <!> Monad.get

    let setStateGraphM stateG = Monad.M (fun (_,stateTS) -> (),(stateG,stateTS))
    let setStateTimeSeriesM stateTS = Monad.M (fun (stateG,_) -> (),(stateG,stateTS))

    let runGraphM m = 
        Monad.state {
            let! (r,s) = Monad.run m <!> stateGraphM ()
            do! setStateGraphM s
            return r
        }
                               
    let runTimeSeriesM m = 
        Monad.state {
            let! (r,s) = Monad.run m <!> stateTimeSeriesM ()
            do! setStateTimeSeriesM s
            return r
        }
    
    let sampleOnceM updateM skM = 
        Monad.state {
            let! x = runGraphM skM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentElementM x)
            let! innov = runGraphM Graph.innovationsM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentInnovationM innov.[0])
            do! runTimeSeriesM TimeSeries.Univariate.stepM
            let! newVar = runTimeSeriesM updateM
            do! runGraphM (Graph.setVariablesM newVar)
            return x
        }

    let currentErrorM skM = 
        Monad.state {
            let! x = runGraphM skM
            let! currentElement = runTimeSeriesM (TimeSeries.Univariate.currentElementM ())
            return (currentElement |> Option.defaultValue 0.0) - x
        } 

    let fitOnceM updateM skM = 
        Monad.state {
            let! currentError = currentErrorM skM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentInnovationM currentError)
            do! runTimeSeriesM TimeSeries.Univariate.stepM
            let! newVar = runTimeSeriesM updateM
            do! runGraphM (Graph.setVariablesM newVar)
        }

    let SDGfitM updateM skM idx = 
        Monad.state {
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentIndexM idx)
            let! newVar = runTimeSeriesM updateM
            do! runGraphM (Graph.setVariablesM newVar)
            let! x = runGraphM skM
            let! currentError = currentErrorM skM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentInnovationM currentError)
        }

    let sample n = function
        | Sampling(mparams) -> let initStateG = Graph.defaultState (Sampling(mparams))
                               let initStateTS = TimeSeries.Univariate.defaultState n
                               let skM = Graph.modelM (Sampling(mparams))
                               let updateM = defineUpdatesM mparams
                               let sampleM = sampleOnceM updateM skM
                               let sampleArrayM = Array.zeroCreate n |> Array.map (fun _ -> sampleM)
                               Monad.run (Monad.mapM sampleArrayM) (initStateG,initStateTS) |> fst
        | Fitting(m) -> invalidArg "model" "Cannot sample with a Fitting model type. Convert it to a Sampling type."
                   
    let getError model array stateG = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let skM = Graph.modelM (Fitting(model))
        let updateM = updateForFittingM model
        let fittingM = fitOnceM updateM skM
        let fittingArrayM = Array.zeroCreate (array.Length) |> Array.map (fun _ -> fittingM)
        let (_,TimeSeries.Univariate.State(_,_,errors)) = Monad.run (Monad.mapM fittingArrayM) (stateG,initStateTS) |> snd
        errors |> Array.map (fun x -> Option.defaultValue 0.0 x)
       