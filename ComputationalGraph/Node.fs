namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    // Should be modified to allow for different group
    // Or shoud implement a way to shift group.
    let inline linearCombinaison n grpIdx : Graph<'T> = 
        Seq.init n (fun idx -> Input(Parameter(0,idx)) * Input(Variable(0,idx))) |> sum |> Graph<'T>.changeGroup 0 grpIdx

