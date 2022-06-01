namespace Models

open FSharpPlus
open FSharpPlus.Data
open ModelType
open ComputationalGraph
open ComputationalGraph.GraphType
open Timeseries.TimeseriesType
open Timeseries.TimeseriesState

[<RequireQualifiedAccess>]
module Optimizers = 
    
    type Classic<'T> = 
        { LearningRate:'T }
        static member inline create (rate:'T) = { LearningRate=rate }

    // 'MomentumValue' array dim. must match the number of parameters in the model.
    // Each parameter will have its 'MomentumValue'.
    type Momentum<'T> = 
        { LearningRate:'T; MomentumRate:'T; MomentumValue:'T[,] }
        static member inline create (learn:'T) (mom:'T) (i,j) = { LearningRate=learn; MomentumRate=mom; MomentumValue=Array2D.zeroCreate<'T> i j }

    [<RequireQualifiedAccess>]
    type OptimizerState<'T> = 
        | Classic of Classic<'T>
        | Momentum of Momentum<'T>

    let inline update (parameters:'T[,]) (gradient:'T[,]) (s:OptimizerState<'T>) = 
        match s with
        | OptimizerState.Classic(opt) as x -> x, Array2D.map2 (fun p g -> p - opt.LearningRate * g) parameters gradient
        | OptimizerState.Momentum(opt) -> let newMomentum = Array2D.map2 (fun mom g -> opt.MomentumRate*mom + opt.LearningRate * g ) opt.MomentumValue gradient
                                          OptimizerState<'T>.Momentum({ opt with MomentumValue=newMomentum }), Array2D.map2 (-) parameters newMomentum 


module Optimisation = 
    
    // Optimizer.Method keeps tracks of the updated momentum values. 
    type S<'T when 'T : (static member Zero : 'T)> = S of ModelType.S<'T> * Optimizers.OptimizerState<'T>

    [<RequireQualifiedAccess>]
    type Optimizer<'T when 'T : (static member Zero : 'T)> = 
        | Classic of learnRate:'T
        | Momentum of learnRate:'T * momentumRate:'T
        static member inline convert (m:Model<'T>) (opt:Optimizer<'T>) = 
            match opt with
            | Classic(r) -> Optimizers.Classic.create r |> Optimizers.OptimizerState.Classic
            | Momentum(r,mom) -> Model.parameterShape m |> Optimizers.Momentum.create r mom |> Optimizers.OptimizerState.Momentum


    let inline getModelState (S(ms,_)) = ms 
    let inline getOptimizerState (S(_,opt)) = opt

    let inline modelState () = getModelState <!> State.get
    let inline optimizerState () = getOptimizerState <!> State.get

    let inline evalM (modelM:State<ModelType.S<'T>,'a>) = State.eval modelM <!> modelState()
    let inline modifyM modelM = State.exec modelM <!> modelState() >>= (fun newM -> State.modify (fun (S(_,oldMethod)) -> S(newM,oldMethod)))

    // Fit the model with the given optimizer.
    // Model should already contain the data and be a kind of 'ErrorModel'. 
    let inline fit (errModel:Model<'T>) (opt:Optimizer<'T>) (epochs:int) (ts:TS<'T>) =
        let initStateOptimizer = Optimizer.convert errModel opt
        let initStateModel = Model.defaultEmptyState ts errModel
        let initState = S(initStateModel, initStateOptimizer)

        // Updating of the parameters during optimization according to the chosen Optimizer.
        // Will compute the gradient at the current timestep. 
        let updateParameters = monad {
            let! p = (ModelState.evalG >> evalM) (GraphState.parametersM())
            let! gradients = (ModelState.evalG >> evalM) errModel.GraphGradient
            let! newOpt, newParams = Optimizers.update p gradients <!> optimizerState()
            do! State.modify (fun (S(ms,_)) -> S(ms,newOpt))
            do! (ComputationalGraph.GraphState.updateParameters >> ModelState.modifyG >> modifyM) newParams
        }

        // The value of the objective function is given in the innovation/errors 'TS'. NOT the in-sample errors !
        let fitOnIdx idx = monad {
            do! (Timeseries.TimeseriesState.setTime >> ModelState.modifyT >> modifyM) idx
            do! (ModelState.updateVariables >> modifyM) errModel.UpdateRule
            let! objFunc = (ModelState.evalG >> evalM) errModel.GraphMonad
            do! (Timeseries.TimeseriesState.setCurrentElements >> ModelState.modifyI >> modifyM) objFunc
            do! updateParameters
        }
        
        Seq.randomIndices 0 ts.Length |> Seq.repeat epochs
                                      |> Seq.map fitOnIdx
                                      |> State.Seq.accumulate
                                      |> flip State.exec initState
        

        
        





