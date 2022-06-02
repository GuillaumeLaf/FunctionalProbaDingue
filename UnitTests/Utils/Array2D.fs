namespace UtilsTest

module Array2D = 
    open Xunit
    open Timeseries
    open FSharpPlus.Data
    open FSharpPlus

    let arr = Array2D.init 3 3 (fun i j -> i*3+j)

    [<Fact>]
    let ``length`` () = Assert.Equal(9, Array2D.length arr)

    [<Fact>]
    let ``shape`` () = Assert.Equal((3,3), Array2D.shape arr)

    [<Fact>]
    let ``row`` () = Assert.Equal<int[]>([|0;1;2|], Array2D.row 0 arr)

    [<Fact>]
    let ``sub`` () = Assert.Equal<int[,]>(array2D[|[|1;2|];[|4;5|];[|7;8|]|], Array2D.sub 1 2 arr)

    [<Fact>]
    let ``setRow`` () = Assert.Equal<int[,]>(array2D [|[|1;1;1|];[|3;4;5|];[|6;7;8|]|],Array2D.setRow 0 [|1;1;1|] arr)

    [<Fact>]
    let ``setColumn`` () = Assert.Equal<int[,]>(array2D [|[|1;1;2|];[|1;4;5|];[|1;7;8|]|],Array2D.setColumn 0 [|1;1;1|] arr)

    [<Fact>]
    let ``collectByRow`` () = Assert.Equal<int[]>([|3;12;21|], (Array.fold (+) 0 |> Array2D.collectByRow) arr) 

    [<Fact>]
    let ``collectByCol`` () = Assert.Equal<int[]>([|9;12;15|], (Array.fold (+) 0 |> Array2D.collectByCol) arr) 

    [<Fact>]
    let ``singleton`` () = Assert.Equal<int[,]>(array2D [|[|1|];[|2|];[|3|]|], Array2D.singleton [|1;2;3|])

    [<Fact>]
    let ``ofSingleArray``() = Assert.Equal<int[,]>(array2D [|[|0;1;2|];[|3;4;5|]|], Array2D.ofSingleArray (2,3) [|0;1;2;3;4;5|])

    [<Fact>]
    let ``flatten`` () = Assert.Equal<int[]>(Array.init 9 id, Array2D.flatten arr)
        
    [<Fact>]
    let ``stackColumn`` () = 
        let expected = array2D [|[|0;1;2;0;1;2|];[|3;4;5;3;4;5|];[|6;7;8;6;7;8|]|]
        Assert.Equal<int[,]>(expected, Array2D.stackColumn arr arr)

    [<Fact>]
    let ``stackRow`` () = 
        let expected = array2D [|[|0;1;2|];[|3;4;5|];[|6;7;8|];[|0;1;2|];[|3;4;5|];[|6;7;8|]|]
        Assert.Equal<int[,]>(expected, Array2D.stackRow arr arr)

    [<Fact>]
    let ``map2`` () = Assert.Equal<int[,]>(array2D [|[|0;2;4|];[|6;8;10|];[|12;14;16|]|], Array2D.map2 (+) arr arr)

    [<Fact>]
    let ``toOption`` () = Assert.Equal<int option[,]>(array2D [|[|Some 0;Some 1;Some 2|];[|Some 3;Some 4;Some 5|];[|Some 6;Some 7;Some 8|]|], Array2D.toOption arr)










