namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    let inline linearCombinaison n : Graph<'T> = 
        Seq.init n (fun idx -> DataPoint(Parameter(0,idx)) * DataPoint(Variable(0,idx))) |> sum

