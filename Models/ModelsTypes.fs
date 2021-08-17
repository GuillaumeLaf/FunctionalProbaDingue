namespace Models

[<AutoOpen>]
module ModelsTypes = 

    type Op = 
        | Addition
        | Multiplication
        | Substraction
        | LessThan // return 1.0 if Left node is less than Rigth node else 0.0 : (l < r)

    type Input = 
        | Innovation
        | Parameter
        | Variable
        | PreviousResult
        | Constant

    type Skeleton =     // Contains the skeleton of the model. The data of the Parameters and Variables are stored in arrays.
        | Leaf of Input * x:float option * idx:int * pullFrom: (Input * int) option
        | Node of Op * Skeleton * Skeleton

    type GraphState = GraphState of p:float array * v:float array * innov:float array * prevResult:float array * constant:float array

    type Graph = Graph of GraphState * Skeleton

    type UpdateVariableStrategy = UpdateVariableStrategy of pullFrom:(Input * int) option array

    type ModelName = 
        | MA
        | AR
        | SETAR

    type ModelParameters = 
        | MAparams of coeffs:float array
        | ARparams of coeffs:float array
        | SETARparams of coeffs1:float array * coeffs2:float array * threshold:float * delay:int

    type ModelType = ModelType of ModelName * ModelParameters

    type T = T of ModelName * Graph * UpdateVariableStrategy