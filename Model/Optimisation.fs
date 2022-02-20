namespace Models

open FSharpPlus
open FSharpPlus.Data
open ModelType
open ModelState

[<RequireQualifiedAccess>]
module Optimizers = 
    
    type Classic = 
        { LearningRate:float32 }
        static member create rate = { LearningRate=rate }

    type Momentum = 
        { LearningRate:float32; MomentumRate:float32; MomentumValue:float32[] }
        static member create learn mom n = { LearningRate=learn; MomentumRate=mom; MomentumValue=Array.zeroCreate n }

    type Method = 
        | Classic of Classic
        | Momentum of Momentum

(*module ErrorTypes = 
    type Error =
        | Raw
        | Squared*)

module Optimisation = 

    type Optimizer = 
        | Classic of learnRate:float32
        | Momentum of learnRate:float32 * momentumRate:float32

    type S = S of ModelType.S * Optimizers.Method


    let getModelState (S(ms,_)) = ms 

    let modelState () = getModelState <!> State.get

    let evalM (modelM:State<ModelType.S,'a>) = State.eval modelM <!> modelState()
    let modifyM modelM = State.exec modelM <!> modelState() >>= (fun newM -> State.modify (fun (S(_,oldMethod)) -> S(newM,oldMethod)))

    // Fit the model with the given optimizer.
    // Model should already contain the data.
    let fit (errModel:Model) (opt:Optimizer) = 
        
        

        






