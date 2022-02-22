namespace Models

open FSharpPlus
open FSharpPlus.Data
open ModelType
open ModelState
open ComputationalGraph.GraphType
open ComputationalGraph.GraphState
open Timeseries.TimeseriesType
open Timeseries.TimeseriesState

[<RequireQualifiedAccess>]
module Optimizers = 
    
    type Classic = 
        { LearningRate:float32 }
        static member create rate = { LearningRate=rate }

    // 'MomentumValue' array dim. must match the number of parameters in the model.
    // Each parameter will have its 'MomentumValue'.
    type Momentum = 
        { LearningRate:float32; MomentumRate:float32; MomentumValue:float32[,] }
        static member create learn mom (i,j) = { LearningRate=learn; MomentumRate=mom; MomentumValue=Array2D.zeroCreate i j }

    [<RequireQualifiedAccess>]
    type Method = 
        | Classic of Classic
        | Momentum of Momentum

    let update parameters gradient = function
        | Method.Classic(opt) as x -> x, Array2D.map2 (fun p g -> p - opt.LearningRate * g) parameters gradient
        | Method.Momentum(opt) -> let newMomentum = Array2D.map2 (fun mom g -> opt.MomentumRate*mom + opt.LearningRate*g ) opt.MomentumValue gradient
                                  Method.Momentum({ opt with MomentumValue=newMomentum }), Array2D.map2 ( - ) parameters newMomentum
        
(*module ErrorTypes = 
    type Error =
        | Raw
        | Squared*)

module Optimisation = 

    type S = S of ModelType.S * Optimizers.Method

    [<RequireQualifiedAccess>]
    type Optimizer = 
        | Classic of learnRate:float32
        | Momentum of learnRate:float32 * momentumRate:float32
        static member convert (N,T) = function
            | Classic(r) -> Optimizers.Classic.create r |> Optimizers.Method.Classic
            | Momentum(r,mom) -> Optimizers.Momentum.create r mom (N,T) |> Optimizers.Method.Momentum


    let getModelState (S(ms,_)) = ms 
    let getOptimizerState (S(_,opt)) = opt

    let modelState () = getModelState <!> State.get
    let optimizerState () = getOptimizerState <!> State.get

    let evalM (modelM:State<ModelType.S,'a>) = State.eval modelM <!> modelState()
    let modifyM modelM = State.exec modelM <!> modelState() >>= (fun newM -> State.modify (fun (S(_,oldMethod)) -> S(newM,oldMethod)))

    // Fit the model with the given optimizer.
    // Model should already contain the data.
    let fit (errModel:Model) (opt:Optimizer) =
        let initStateOptimizer = Optimizer.convert (errModel.N,errModel.T) opt
        let initStateModel = ModelState.defaultState errModel
        let initState = S(initStateModel, initStateOptimizer)

        // Updating of the parameters during optimization according to the chosen Optimizer.
        // Will compute the gradient at the current timestep. 
        let updateParameters = monad {
            let! p = (ModelState.evalG >> evalM) ComputationalGraph.GraphState.parametersM
            let! gradients = (ModelState.evalG >> evalM) errModel.GraphGradient
            let! newOpt, newParams = Optimizers.update p gradients <!> optimizerState()
            do! State.modify (fun (S(ms,_)) -> S(ms,newOpt))
            do! (ComputationalGraph.GraphState.updateParameters >> ModelState.evalG >> modifyM) newParams
        }

        let fitOnIdx idx = monad {
            do! (Timeseries.TimeseriesState.setTime >> ModelState.modifyT >> modifyM) idx
            do! (ModelState.updateVariables >> modifyM) errModel.UpdateRule
            let! errors = (ModelState.evalG >> evalM) errModel.GraphMonad
            do! (Timeseries.TimeseriesState.setCurrentElements >> ModelState.modifyI >> modifyM) errors
            do! updateParameters
        }
        
        Array.init errModel.T (fun idx -> errModel.T-idx) |> Array.map fitOnIdx
                                                          |> State.traverseBack
                                                          |> flip State.exec initState
        

        






