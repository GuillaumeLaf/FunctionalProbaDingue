namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    // Special nodes groups represents basic templates.
    // For instance, 'Group Indices' should be normally be zero and then later modified.

    type Vector(s:seq<Graph>) =
        let vec = s
        member this.seq = vec
        member this.ToArray = Array.ofSeq vec
        static member init basicinput n = Seq.init n (fun idx -> Input(basicinput(0,idx))) |> Vector
        static member (+) (v1:Vector, v2:Vector) = Seq.map2 (+) v1.seq v2.seq |> Vector

    type Matrix<'T> = Matrix of seq<seq<'T>>

    // InnerProduct of two arrays of 'Graph' objects
    let inline innerProduct arrG1 arrG2 = (arrG1, arrG2) ||> Seq.map2 (fun x1 x2 -> x1 * x2) |> sum   
    
    // Dot Product of a matrix 'A' of 'Parameter's (as 'Input') and a vector of 'Variable's (as 'Input).
    // Should be extended to take the dot product of 2 matrices.
    let inline dotProduct A b = Seq.map (fun aRow -> innerProduct aRow b) A

    // Construct a sequence of 'BasicInput' types
    // 'Start_' ('end_') specifies the first (last) index of the 'BasicInput'.
    // 'end_' is included.
    let basicArray basicInput grpIdx start_ end_ = Seq.init (end_-start_+1) (fun idx -> Input(basicInput(grpIdx,idx+start_))) |> Seq.cache

    // Specialized function to construct 'BasicInput' types
    let ParameterVector = basicArray Parameter
    let VariableVector = basicArray Variable

    let inline linearCombinaison grpIdx start_ end_ = innerProduct (ParameterVector grpIdx start_ end_) (VariableVector grpIdx start_ end_)

    let inline multivariateLinearCombinaison lag n = 
        dotProduct (Seq.init n (fun i -> Seq.init n (fun j -> Input(Parameter(j,i*n+j))))) (Seq.init n (fun idx -> Input(Variable(idx,lag))))



