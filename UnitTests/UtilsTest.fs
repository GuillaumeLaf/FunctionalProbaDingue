module UtilsTest
    open Xunit
    open Timeseries
    open FSharpPlus.Data
    open FSharpPlus

    let arr = Array2D.init 3 3 (fun i j -> i*3+j)

    [<Fact>]
    let ``Length of array2d`` () = Assert.Equal(9, Array2D.length arr)

    [<Fact>]
    let ``Convert Jagged Array into Array2D`` () = Assert.Equal<int[,]>(arr, Array2D.ofArray [|[|0;1;2|];[|3;4;5|];[|6;7;8|]|])

    [<Fact>]
    let ``Get a given Row`` () = Assert.Equal<int[]>([|0;1;2|], Array2D.row 0 arr)

    [<Fact>]
    let ``Set a given Row`` () = Assert.Equal<int[,]>(Array2D.ofArray [|[|1;1;1|];[|3;4;5|];[|6;7;8|]|],Array2D.setRow 0 [|1;1;1|] arr)

    [<Fact>]
    let ``Set a given Column`` () = Assert.Equal<int[,]>(Array2D.ofArray [|[|1;1;2|];[|1;4;5|];[|1;7;8|]|],Array2D.setColumn 0 [|1;1;1|] arr)

    [<Fact>]
    let ``Apply a function onto each row and collect the result in an 'Array'`` () = Assert.Equal<int[]>([|3;12;21|], (Array.fold (+) 0 |> Array2D.collectByRow) arr) 

    [<Fact>]
    let ``Convert an 'Array' into an Array2D Singleton`` () = Assert.Equal<int[,]>(Array2D.ofArray [|[|1|];[|2|];[|3|]|], Array2D.singleton [|1;2;3|])

    [<Fact>]
    let ``ofSingleArray``() = Assert.Equal<int[,]>(Array2D.ofArray [|[|0;1;2|];[|3;4;5|]|], Array2D.ofSingleArray (2,3) [|0;1;2;3;4;5|])

    [<Fact>]
    let ``Flatten into an 'Array'`` () = Assert.Equal<int[]>(Array.init 9 id, Array2D.flatten arr)
        
    [<Fact>]
    let ``Concatenate two 'Array2D's column-wise`` () = 
        let expected = Array2D.ofArray [|[|0;1;2;0;1;2|];[|3;4;5;3;4;5|];[|6;7;8;6;7;8|]|]
        Assert.Equal<int[,]>(expected, Array2D.stackColumn arr arr)

    [<Fact>]
    let ``Concatenate two 'Array2D's row-wise`` () = 
        let expected = Array2D.ofArray [|[|0;1;2|];[|3;4;5|];[|6;7;8|];[|0;1;2|];[|3;4;5|];[|6;7;8|]|]
        Assert.Equal<int[,]>(expected, Array2D.stackRow arr arr)

    [<Fact>]
    let ``Map two 'Array2D's into one`` () = Assert.Equal<int[,]>(Array2D.ofArray [|[|0;2;4|];[|6;8;10|];[|12;14;16|]|], Array2D.map2 (+) arr arr)