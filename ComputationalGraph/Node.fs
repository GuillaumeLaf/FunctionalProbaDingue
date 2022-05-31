namespace ComputationalGraph

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open GraphType

[<RequireQualifiedAccess>]
module Node = 
    // Special nodes groups represents basic templates.

    // Type to differentiate the 'fixed' index type when instantiating a 'Vector' or a 'Matrix'.
    // For instance, 'Group' fixes the group index of the vector and allow to change the standard index freely.
    type FixedIdx = 
        | Standard of int
        | Group of int

    type Vector<'T> = 
        { Size:int; Graphs:Graph<'T>[] }
        static member inline ( + ) (v1:Vector<'T>, v2:Vector<'T>) = if v1.Size = v2.Size then { v1 with Graphs=Array.map2 ( + ) v1.Graphs v2.Graphs } else invalidArg (nameof v1) "Cannot return the addition, 'Vectors' don't have the same size."
        static member inline ( * ) (v1:Vector<'T>, v2:Vector<'T>) = if v1.Size = v2.Size then { v1 with Graphs=Array.map2 ( * ) v1.Graphs v2.Graphs } else invalidArg (nameof v1) "Cannot return the multiplication, 'Vectors' don't have the same size."
        

    // Dimensions of the matrix (Length of 'Vector's, number of vectors)
    // Therefore, the vectors are stacked column-wise
    type Matrix<'T> = 
        { Size:int*int; Vectors:Vector<'T>[]}
        static member inline ( + ) (m1:Matrix<'T>, m2:Matrix<'T>) = if m1.Size = m2.Size then { m1 with Vectors=Array.map2 ( + ) m1.Vectors m2.Vectors } else invalidArg (nameof m1) "Cannot return the addition, 'Matrix' don't have the same size."
        static member inline ( * ) (m1:Matrix<'T>, m2:Matrix<'T>) = if m1.Size = m2.Size then { m1 with Vectors=Array.map2 ( * ) m1.Vectors m2.Vectors } else invalidArg (nameof m1) "Cannot return the multiplication, 'Matrix' don't have the same size."

    [<RequireQualifiedAccess>]
    module Vector = 
        
        let inline create size graphs = {Size=size; Graphs=graphs}
        
        // Create a 'Vector' from an array of 'Graph's. 'Size' of the 'Vector' is the length of the given array. 
        let inline createFrom (graphs:Graph<'T>[]) = create graphs.Length graphs

        let inline graphs (v:Vector<'T>) = v.Graphs
        let inline size (v:Vector<'T>) = v.Size

        let inline private returnIfEqualSize ((v1,v2):Vector<'T>*Vector<'T>) result = if v1.Size = v2.Size then result else invalidArg "Vector" "Cannot return the result, 'Vectors' don't have the same size."

        let equal (v1:Vector<'T>) (v2:Vector<'T>) = Array.forall2 Graph.equal v1.Graphs v2.Graphs

        // Create a new 'Graph' by summing 'Graph's in the 'Vector'
        let sum (v:Vector<'T>) = graphs >> Array.reduce Graph.add <| v

        let add (v1:Vector<'T>) (v2:Vector<'T>) = { v1 with Graphs=Array.map2 Graph.add v1.Graphs v2.Graphs } |> returnIfEqualSize (v1,v2) 
        let multiply (v1:Vector<'T>) (v2:Vector<'T>) = { v1 with Graphs=Array.map2 Graph.multiply v1.Graphs v2.Graphs } |> returnIfEqualSize (v1,v2) 
        
        // Dot product of two 'Vector's, thus creating a 'Graph'.
        let dotProduct (v1:Vector<'T>) (v2:Vector<'T>) =  multiply v1 v2 |> sum

        // Create a standard 'Vector' of graph 'Parameter' or graph 'Variable' 
        // basicInput : specifies the 'Graph.Inputs.BasicInput' type 
        // idxType : choose the fixed index
        // shift : choose the shift to be applied to the changing index.
        // n : length of the 'Vector'
        // For instance, if 'Group' is the fixed type then the 'Standard' index will
        // vary over 'shift'...'n'+'shift'.
        let initShifted basicInput fixedType shift n = 
            match fixedType with
            | Standard(idx) -> { Size=n; Graphs=[| for i in 0..n-1 do Input(basicInput(i+shift,idx)) |] }
            | Group(idx) -> { Size=n; Graphs=[| for i in 0..n-1 do Input(basicInput(idx,i+shift)) |] }

        // Specialized version of 'initShifted' function where
        // the shift is set to 0.
        let init basicInput idxType n  = initShifted basicInput idxType 0 n
        
        let normL2 (v:Vector<'T>) = graphs >> Array.map (fun g -> Polynomial(g,2)) >> createFrom >> sum <| v
        
    [<RequireQualifiedAccess>]
    module Matrix = 

        let inline create size vectors = {Size=size; Vectors=vectors}

        let inline vectors (m:Matrix<'T>) = m.Vectors
        let inline size (m:Matrix<'T>) = m.Size
        let inline size1 (m:Matrix<'T>) = size >> fst <| m
        let inline size2 (m:Matrix<'T>) = size >> snd <| m

        let inline private returnIfEqualSize ((m1,m2):Matrix<'T>*Matrix<'T>) result = if m1.Size = m2.Size then result else invalidArg "Matrix" "Cannot return the result, 'Matrix' don't have the same size."

        // Two 'Matrix' are equal when all their 'Vector's are equal
        let equal (m1:Matrix<'T>) (m2:Matrix<'T>) = Array.forall2 Vector.equal m1.Vectors m2.Vectors |> returnIfEqualSize (m1,m2)

        // Create a standard 'Matrix' of graph 'Parameter' or graph 'Variable' 
        // basicInput : specifies the 'Graph.Inputs.BasicInput' type 
        // shift : choose the shift to be applied to the changing index.
        // (i,j) : number of rows and columns
        // Note : each column/'Vector' has fixed 'Group' index.
        let initShifted basicInput shift (i,j) : Matrix<'T> = {Size=(i,j); Vectors=[| for grpidx in 0..j-1 do Vector.initShifted basicInput (Group(grpidx)) shift i |]}

        // Specialize 'initShifted' function where
        // 'shift' parameter is set to 0.
        let init basicInput (i,j) : Matrix<'T> = initShifted basicInput 0 (i,j)

        // Matrix transpose
        let transpose (m:Matrix<'T>) = {Size=(snd m.Size, fst m.Size); 
                           Vectors=(Array.map Vector.graphs >> Array.transpose >> Array.map (Vector.create (size2 m))) m.Vectors }
              
        // Create a 'Matrix' by elementwise addition of two 'Matrix'.
        let add (m1:Matrix<'T>) (m2:Matrix<'T>) = { m1 with Vectors=Array.map2 Vector.add m1.Vectors m2.Vectors } |> returnIfEqualSize (m1,m2)
        
        // Create a 'Matrix' by elementwise multiplication of two 'Matrix'.
        let multiply (m1:Matrix<'T>) (m2:Matrix<'T>) = { m1 with Vectors=Array.map2 Vector.multiply m1.Vectors m2.Vectors } |> returnIfEqualSize (m1,m2)
        
        // Create a 'Vector' by the dot product of a 'Vector' and a 'Matrix'.
        let leftVectorProduct (v:Vector<'T>) (m:Matrix<'T>) = 
            if fst m.Size = v.Size then 
                m |> (vectors >> Array.map (fun vm -> Vector.dotProduct vm v) >> Vector.create (size2 m) ) 
            else invalidArg (nameof m) "Cannot dotproduct matrix and vector of different dimensions."
                  
        // Create a 'Vector' by the dot product of a 'Matrix' and a 'Vector'.
        let rightVectorProduct (m:Matrix<'T>) (v:Vector<'T>) = m |> (transpose >> leftVectorProduct v)

        // Create a 'Matrix' by the dot product of two 'Matrix'.
        let matrixProduct (m1:Matrix<'T>) (m2:Matrix<'T>) = 
            if snd m1.Size = fst m2.Size then 
                transpose { Size=(fst m1.Size, snd m2.Size); Vectors= m1 |> (transpose >> vectors >> Array.map (fun vm1 -> leftVectorProduct vm1 m2)) }
            else invalidArg "Matrix" "Cannot MatrixProduct matrices with wrong dimensions."
        
    
    // Create a 'Graph' from the linear combinaison of 'Parameter's and 'Variable's.
    // grpIdx : 'Group' index for every element of the linear combinaison.
    // startLag_, endLag_ : the first (inclu.) and last (inclu.) 'Standard' index for each element ('Parameter' and 'Variable')
    let inline linearCombinaison grpIdx startLag_ endLag_ : Graph<'T> = Vector.dotProduct (Vector.initShifted Parameter (Group(grpIdx)) startLag_ (endLag_-startLag_+1))
                                                                              (Vector.initShifted Variable (Group(grpIdx)) startLag_ (endLag_-startLag_+1))
        
    // Create a 'Array' of 'Graph's from the sum of dotproducts.
    // For instance, Ax + By + ...
    let inline multivariateLinearCombinaison startLag_ endLag_ n : Graph<'T>[] = 
        let AtLag l = Matrix.leftVectorProduct (Vector.init Variable (Standard(l)) n) (Matrix.initShifted Parameter ((l-startLag_)*n) (n,n)) 
        [|startLag_..endLag_|] |> Array.map AtLag
                               |> Array.reduce Vector.add
                               |> Vector.graphs

    
