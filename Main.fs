
open System
open FSharpPlus.Data
open FSharpPlus
open ComputationalGraph.Graph
open ComputationalGraph.Node

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let m1 = Matrix.init Parameter (2,3)
    let m2 = Matrix.init Parameter (3,2)
    let actual = m1 */ m2
    let a11 = Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(0,1))*Input(Parameter(1,0))+Input(Parameter(0,2))*Input(Parameter(2,0))
    let a12 = Input(Parameter(0,0))*Input(Parameter(0,1))+Input(Parameter(0,1))*Input(Parameter(1,1))+Input(Parameter(0,2))*Input(Parameter(2,1))
    let a21 = Input(Parameter(1,0))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(1,0))+Input(Parameter(1,2))*Input(Parameter(2,0))
    let a22 = Input(Parameter(1,0))*Input(Parameter(0,1))+Input(Parameter(1,1))*Input(Parameter(1,1))+Input(Parameter(1,2))*Input(Parameter(2,1))
    let expected = [| Vector([|a11;a12|]); Vector([|a21;a22|]) |] |> Matrix
    printfn "%A" (actual.Vectors.[1].Value)


    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code

