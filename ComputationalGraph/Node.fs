namespace ComputationalGraph

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open Graph
open GraphType

module Node = 
    // Special nodes groups represents basic templates.

    // Type to differentiate the 'fixed' index type when instantiating a 'Vector' or a 'Matrix'.
    // For instance, 'Group' fixes the group index of the vector and allow to change the standard index freely.
    type FixedIdx = 
        | Standard of int
        | Group of int

    type Vector = { size:int; graphs:Graph[] }

    // Dimensions of the matrix (Length of 'Vector's, number of vectors)
    // Therefore, the vectors are stacked column-wise
    type Matrix = { size:int*int; vectors:Vector[]}

    [<RequireQualifiedAccess>]
    module Vector = 
        
        let create size graphs = {size=size; graphs=graphs}

        let graphs (v:Vector) = v.graphs
        let size (v:Vector) = v.size

        let inline private returnIfEqualSize ((v1,v2):Vector*Vector) result = if v1.size = v2.size then result else invalidArg "Vector" "Cannot return the result, 'Vectors' don't have the same size."

        let equal (v1:Vector) (v2:Vector) = Array.forall2 Graph.equal v1.graphs v2.graphs

        let sum = graphs >> Array.reduce Graph.add

        let add (v1:Vector) (v2:Vector) = { v1 with graphs=Array.map2 Graph.add v1.graphs v2.graphs } |> returnIfEqualSize (v1,v2) 
        let multiply (v1:Vector) (v2:Vector) = { v1 with graphs=Array.map2 Graph.multiply v1.graphs v2.graphs } |> returnIfEqualSize (v1,v2) 
        
        // Dot product of two 'Vector's, thus creating a 'Graph'.
        let dotProduct =  (uncurry multiply) >> sum |> curry

        // Create a standard 'Vector' of graph 'Parameter' or graph 'Variable' 
        // basicInput : specifies the 'Graph.Inputs.BasicInput' type 
        // idxType : choose the fixed index
        // shift : choose the shift to be applied to the changing index.
        // n : length of the 'Vector'
        // For instance, if 'Group' is the fixed type then the 'Standard' index will
        // vary over 'shift'...'n'+'shift'.
        let initShifted basicInput idxType shift n = 
            match idxType with
            | Standard(idx) -> { size=n; graphs=[| for i in 0..n-1 do Input(basicInput(i+shift,idx)) |] }
            | Group(idx) -> { size=n; graphs=[| for i in 0..n-1 do Input(basicInput(idx,i+shift)) |] }

        // Specialized version of 'initShifted' function where
        // the shift is set to 0.
        let init basicInput idxType n  = initShifted basicInput idxType 0 n
        
    [<RequireQualifiedAccess>]
    module Matrix = 

        let create size vectors = {size=size; vectors=vectors}

        let vectors (m:Matrix) = m.vectors
        let size (m:Matrix) = m.size
        let size1 = size >> fst
        let size2 = size >> snd

        let inline private returnIfEqualSize ((m1,m2):Matrix*Matrix) result = if m1.size = m2.size then result else invalidArg "Matrix" "Cannot return the result, 'Matrix' don't have the same size."

        // Two 'Matrix' are equal when all their 'Vector's are equal
        let equal (m1:Matrix) (m2:Matrix) = Array.forall2 Vector.equal m1.vectors m2.vectors |> returnIfEqualSize (m1,m2)

        // Create a standard 'Matrix' of graph 'Parameter' or graph 'Variable' 
        // basicInput : specifies the 'Graph.Inputs.BasicInput' type 
        // shift : choose the shift to be applied to the changing index.
        // (i,j) : number of rows and columns
        // Note : each column/'Vector' has fixed 'Group' index.
        let initShifted basicInput shift (i,j) = {size=(i,j); vectors=[| for grpidx in 0..j-1 do Vector.initShifted basicInput (Group(grpidx)) shift i |]}

        // Specialize 'initShifted' function where
        // 'shift' parameter is set to 0.
        let init basicInput (i,j) = initShifted basicInput 0 (i,j)

        // Matrix transpose
        let transpose m = {size=(snd m.size, fst m.size); 
                           vectors=(Array.map Vector.graphs >> Array.transpose >> Array.map (Vector.create (size2 m))) m.vectors }
              
        // Create a 'Matrix' by elementwise addition of two 'Matrix'.
        let add (m1:Matrix) (m2:Matrix) = { m1 with vectors=Array.map2 Vector.add m1.vectors m2.vectors } |> returnIfEqualSize (m1,m2)
        
        // Create a 'Matrix' by elementwise multiplication of two 'Matrix'.
        let multiply (m1:Matrix) (m2:Matrix) = { m1 with vectors=Array.map2 Vector.multiply m1.vectors m2.vectors } |> returnIfEqualSize (m1,m2)
        
        // Create a 'Vector' by the dot product of a 'Vector' and a 'Matrix'.
        let LeftVectorProduct (v:Vector) (m:Matrix) = 
            if fst m.size = v.size then 
                m |> (vectors >> Array.map (fun vm -> Vector.dotProduct vm v) >> Vector.create (size2 m) ) 
            else invalidArg "Matrix" "Cannot dotproduct matrix and vector of different dimensions."
                  
        let RightVectorProduct (m:Matrix) (v:Vector) = m |> (transpose >> LeftVectorProduct v)

        // Create a 'Matrix' by the dot product of two 'Matrix'.
        let MatrixProduct (m1:Matrix) (m2:Matrix) = 
            if snd m1.size = fst m2.size then 
                transpose { size=(fst m1.size, snd m2.size); vectors= m1 |> (transpose >> vectors >> Array.map (fun vm1 -> LeftVectorProduct vm1 m2)) }
            else invalidArg "Matrix" "Cannot MatrixProduct matrices with wrong dimensions."
        
    
    // Create a 'Graph' from the linear combinaison of 'Parameter's and 'Variable's.
    // grpIdx : 'Group' index for every element of the linear combinaison.
    // startLag_, endLag_ : the first (inclu.) and last (inclu.) 'Standard' index for each element ('Parameter' and 'Variable')
    let inline linearCombinaison grpIdx startLag_ endLag_ = Vector.dotProduct (Vector.initShifted Parameter (Group(grpIdx)) startLag_ (endLag_-startLag_+1))
                                                                              (Vector.initShifted Variable (Group(grpIdx)) startLag_ (endLag_-startLag_+1))
        
    // Create a 'Array' of 'Graph's from the sum of dotproducts.
    // For instance, Ax + By + ...
    let inline multivariateLinearCombinaison startLag_ endLag_ n = 
        let AtLag l = Matrix.RightVectorProduct (Matrix.initShifted Parameter ((l-startLag_)*n) (n,n)) (Vector.init Variable (Standard(l)) n)
        [|startLag_..endLag_|] |> Array.map AtLag
                               |> Array.reduce Vector.add
                               |> Vector.graphs


