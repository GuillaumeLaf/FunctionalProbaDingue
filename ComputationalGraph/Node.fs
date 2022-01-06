namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    let inline linearCombinaison n : Graph<'T> = 
        Seq.init n (fun idx -> DataPoint(Parameter(idx)) * DataPoint(Variable(idx))) |> sum

