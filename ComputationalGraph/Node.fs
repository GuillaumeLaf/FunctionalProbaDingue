namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    // Should be modified to allow for different group
    // Or shoud implement a way to shift group.
    let inline linearCombinaison n : Graph<'T> = 
        Seq.init n (fun idx -> DataPoint(Parameter(0,idx)) * DataPoint(Variable(0,idx))) |> sum

