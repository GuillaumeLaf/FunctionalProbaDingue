namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    // Special nodes groups represents basic templates.
    // For instance, 'Group Indices' should be normally be zero and then later modified.

    type Vector(arrIn:Graph[]) =
        let arr = arrIn
        let dim = arrIn.Length
        member this.Value = arrIn
        member this.Length = dim
        override this.GetHashCode() = hash (dim, arr)
        override this.Equals(v) = 
            match v with
            | :? Vector as v -> Array.forall2 (=) this.Value v.Value
            | _ -> false
            
        static member value (v:Vector) = v.Value
        static member length (v:Vector) = v.Length
        static member init basicinput grpIdx n = [| for idx in 0..n-1 do Input(basicinput(grpIdx,idx)) |] |> Vector
        static member ( + ) (v1:Vector, v2:Vector) = Array.map2 ( + ) v1.Value v2.Value |> Vector
        static member ( * ) (v1:Vector, v2:Vector) = Array.map2 ( * ) v1.Value v2.Value |> Vector
        static member ( */ ) (v1:Vector, v2:Vector) = v1 * v2 |> Vector.value |> Array.reduce (+)
        

    type Matrix(mIn:Vector[]) as self =
        let m = mIn
        let dim = (mIn.[0].Length,mIn.Length)               // (Vector length, #Vectors) -> Vectors stacked column-wise
        member this.Vectors = mIn
        member this.Size = dim
        member this.T = Matrix.transpose self
        override this.GetHashCode() = hash (dim, mIn)
        override this.Equals(m) = 
            match m with
            | :? Matrix as m -> Array.forall2 (=) this.Vectors m.Vectors
            | _ -> false

        static member vectors (m:Matrix) = m.Vectors
        static member size (m:Matrix) = m.Size
        static member init basicinput (i,j) = [| for grpidx in 0..j-1 do Vector.init basicinput grpidx i |] |> Matrix
        static member transpose = Matrix.vectors >> Array.map (fun v -> v.Value) >> Array.transpose >> Array.map (fun v -> Vector(v)) >> Matrix
        static member ( + ) (m1:Matrix, m2:Matrix) = if m1.Size = m2.Size then Array.map2 ( + ) m1.Vectors m2.Vectors |> Matrix else invalidArg "Matrix" "Cannot add matrices of different dimensions"
        static member ( * ) (m1:Matrix, m2:Matrix) = if m1.Size = m2.Size then Array.map2 ( * ) m1.Vectors m2.Vectors |> Matrix else invalidArg "Matrix" "Cannot mult. matrices of different dimension" 
        static member ( */ ) (v:Vector, m:Matrix) = if fst m.Size = v.Length then m |> (Matrix.vectors >> Array.map (fun vm -> vm */ v) >> Vector) else invalidArg "Matrix" "Cannot dotproduct matrix and vector of different dimensions."
        static member ( */ ) (m:Matrix, v:Vector) = ( */ ) v m.T 
        static member ( */ ) (m1:Matrix, m2:Matrix) = if snd m1.Size = fst m2.Size then m1.T |> (Matrix.vectors >> Array.map (fun vm1 -> vm1 */ m2) >> Matrix >> Matrix.transpose) else invalidArg "Matrix" "Cannot dotproduct matrices with wrong dimensions."

    let inline linearCombinaison grpIdx start_ end_ = 0

    let inline multivariateLinearCombinaison lag n = 0
        



