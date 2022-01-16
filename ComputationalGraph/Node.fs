namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    // Should be modified to allow for different group
    // Or shoud implement a way to shift group.
    let inline linearCombinaison grpIdx n = 
        Seq.init n (fun idx -> Input(Parameter(grpIdx,idx)) * Input(Variable(grpIdx,idx))) |> sum

