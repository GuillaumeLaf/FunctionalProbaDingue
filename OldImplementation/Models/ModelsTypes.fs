namespace Models

[<AutoOpen>]
module ModelsTypes = 

    type Model = 
        | AR of order:int
        | MA of order:int
        | STAR of order:int * location:float * scale:float * model:Model
        | ErrorModel of Model

    type ModelParameters<'T> = 
        | ARp of 'T[]
        | MAp of 'T[]
        | STARp of 'T[] * 'T[] * 'T * 'T * ModelParameters<'T>
        | ErrorModelp of ModelParameters<'T>

    type ModelType<'T> = 
        | Sampling of ModelParameters<'T> 
        | Fitting of Model

    type Op1<'T> = 
        | Exponential
        | Polynomial of exponent:float

    type Op2 = 
        | Addition
        | Multiplication
        | Substraction
        | Division      // The RHS is the denominator.

    type Input<'T> = 
        | Parameter of idx:int 
        | Variable of idx:int
        | Innovation of idx:int
        | Constant of value:'T

    type Skeleton<'T> = 
        | Leaf of Input<'T> 
        | Node1 of Op1<'T> * Skeleton<'T>
        | Node2 of Op2 * Skeleton<'T> * Skeleton<'T>