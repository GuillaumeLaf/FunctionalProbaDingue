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
            | ErrorModelp(innerModelp) -> Array.concat [|[|TimeSeries.Univariate.currentElementM()|]; updateSequenceTSM innerModelp|]
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

    let fitOnceM updateM errorSkM = 
        Monad.state {
            let! error = runGraphM errorSkM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentInnovationM error)
            do! runTimeSeriesM TimeSeries.Univariate.stepM
            let! newVar = runTimeSeriesM updateM
            do! runGraphM (Graph.setVariablesM newVar)
        }

    let SDGfitM updateM errorSkM idx = 
        Monad.state {
            for i in idx do
                do! runTimeSeriesM (TimeSeries.Univariate.setCurrentIndexM i)
                let! newVar = runTimeSeriesM updateM
                do! runGraphM (Graph.setVariablesM newVar)
                let! error = runGraphM errorSkM
                do! runTimeSeriesM (TimeSeries.Univariate.setCurrentInnovationM error)
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
                  
       