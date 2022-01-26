namespace ComputationalGraph

open FSharpPlus.Data
open FSharpPlus
open Graph

module Node = 
    // Special nodes groups represents basic templates.

    // Type to differentiate the 'fixed' index type when instantiating a 'Vector' or a 'Matrix'.
    // For instance, 'Group' fixes the group index of the vector and allow to change the standard index freely.
    type FixedIdx = 
        | Standard of int
        | Group of int

    type Vector(arrIn:Graph[]) =
        // 'Vector' = 'Array' of 'Graph's
        // Using 'Vector's will ease creation of complex 'Graph's.
        let arr = arrIn

        // Number of 'Graph's in a 'Vector'
        let dim = arrIn.Length
        member this.Value = arrIn
        member this.Length = dim
        override this.GetHashCode() = hash (dim, arr)

        // Two 'Vector's are equal if every 'Graph's are equal.
        override this.Equals(v) = 
            match v with
            | :? Vector as v -> Array.forall2 (=) this.Value v.Value
            | _ -> false
            
        // Get the 'Array' of 'Graph's in the object
        static member toArray (v:Vector) = v.Value
        static member length (v:Vector) = v.Length

        // Create a standard 'Vector' of graph 'Parameter' or graph 'Variable' 
        // basicInput : specifies the 'Graph.Inputs.BasicInput' type 
        // idxType : choose the fixed index
        // shift : choose the shift to be applied to the changing index.
        // n : length of the 'Vector'
        // For instance, if 'Group' is the fixed type then the 'Standard' index will
        // vary over 'shift'...'n'+'shift'.
        static member initShifted basicInput idxType shift n = 
            match idxType with
            | Standard(idx) -> [| for i in 0..n-1 do Input(basicInput(i+shift,idx)) |] |> Vector
            | Group(idx) -> [| for i in 0..n-1 do Input(basicInput(idx,i+shift)) |] |> Vector
                
        // Specialized version of 'initShifted' function where
        // the shift is set to 0.
        static member init basicInput idxType n  = Vector.initShifted basicInput idxType 0 n

        // Create a 'Vector' by elementwise addition of two 'Vector's.
        static member ( + ) (v1:Vector, v2:Vector) = Array.map2 ( + ) v1.Value v2.Value |> Vector

        // Create a 'Vector' by elementwise multiplication of two 'Vector's.
        static member ( * ) (v1:Vector, v2:Vector) = Array.map2 ( * ) v1.Value v2.Value |> Vector

        // Dot product of two 'Vector's, thus creating a 'Graph'.
        static member ( */ ) (v1:Vector, v2:Vector) = v1 * v2 |> Vector.toArray |> Array.reduce (+)
        

    type Matrix(mIn:Vector[]) as self =
        // 'Matrix' = 'Array' of 'Vector's.
        let m = mIn

        // Dimensions of the matrix (Length of 'Vector's, number of vectors)
        // Therefore, the vectors are stacked column-wise
        let dim = (mIn.[0].Length,mIn.Length)               
        member this.Vectors = mIn
        member this.Size = dim

        // Transpose the matrix
        member this.T = Matrix.transpose self
        override this.GetHashCode() = hash (dim, mIn)

        // Two 'Matrix' are equal when ell their 'Vector's are equal
        override this.Equals(m) = 
            match m with
            | :? Matrix as m -> Array.forall2 (=) this.Vectors m.Vectors
            | _ -> false

        // Get the 'Vector's inside the 'Matrix'
        static member vectors (m:Matrix) = m.Vectors
        static member size (m:Matrix) = m.Size

        // Create a standard 'Matrix' of graph 'Parameter' or graph 'Variable' 
        // basicInput : specifies the 'Graph.Inputs.BasicInput' type 
        // shift : choose the shift to be applied to the changing index.
        // (i,j) : number of rows and columns
        // Note : each column/'Vector' has fixed 'Group' index.
        static member initShifted basicInput shift (i,j) = [| for grpidx in 0..j-1 do Vector.initShifted basicInput (Group(grpidx)) shift i |] |> Matrix
        
        // Specialize 'initShifted' function where
        // 'shift' parameter is set to 0.
        static member init basicInput (i,j) = Matrix.initShifted basicInput 0 (i,j)
        
        // Matrix transpose
        static member transpose = Matrix.vectors >> Array.map (fun v -> v.Value) >> Array.transpose >> Array.map (fun v -> Vector(v)) >> Matrix
        
        // Create a 'Matrix' by elementwise addition of two 'Matrix'.
        static member ( + ) (m1:Matrix, m2:Matrix) = if m1.Size = m2.Size then Array.map2 ( + ) m1.Vectors m2.Vectors |> Matrix else invalidArg "Matrix" "Cannot add matrices of different dimensions"
        
        // Create a 'Matrix' by elementwise multiplication of two 'Matrix'.
        static member ( * ) (m1:Matrix, m2:Matrix) = if m1.Size = m2.Size then Array.map2 ( * ) m1.Vectors m2.Vectors |> Matrix else invalidArg "Matrix" "Cannot mult. matrices of different dimension" 
        
        // Create a 'Vector' by the dot product of a 'Vector' and a 'Matrix'.
        static member ( */ ) (v:Vector, m:Matrix) = if fst m.Size = v.Length then m |> (Matrix.vectors >> Array.map (fun vm -> vm */ v) >> Vector) else invalidArg "Matrix" "Cannot dotproduct matrix and vector of different dimensions."
        static member ( */ ) (m:Matrix, v:Vector) = ( */ ) v m.T 
        
        // Create a 'Matrix' by the dot product of two 'Matrix'.
        static member ( */ ) (m1:Matrix, m2:Matrix) = if snd m1.Size = fst m2.Size then m1.T |> (Matrix.vectors >> Array.map (fun vm1 -> vm1 */ m2) >> Matrix >> Matrix.transpose) else invalidArg "Matrix" "Cannot dotproduct matrices with wrong dimensions."

    // Create a 'Graph' from the linear combinaison of 'Parameter's and 'Variable's.
    // grpIdx : 'Group' index for every element of the linear combinaison.
    // startLag_, endLag_ : the first (inclu.) and last (inclu.) 'Standard' index for each element ('Parameter' and 'Variable')
    let inline linearCombinaison grpIdx startLag_ endLag_ = ( */ ) (Vector.initShifted Parameter (Group(grpIdx)) startLag_ (endLag_-startLag_+1))
                                                                   (Vector.initShifted Variable (Group(grpIdx)) startLag_ (endLag_-startLag_+1))
        
    // Create a 'Array' of 'Graph's from the sum of dotproducts.
    // For instance, Ax + By + ...
    let inline multivariateLinearCombinaison startLag_ endLag_ n = 
        let AtLag l = (Matrix.initShifted Parameter ((l-startLag_)*n) (n,n)) */ (Vector.init Variable (Standard(l)) n)
        [|startLag_..endLag_|] |> Array.map AtLag
                               |> Array.reduce (+)
                               |> Vector.toArray


