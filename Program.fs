// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting
open Binance

open DataBase

open Transformations

open MathNet.Numerics
open MathNet.Numerics.IntegralTransforms

[<EntryPoint>]
let main argv =

    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Multiplication, N1, N2)
    let inline ( .-. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Substraction, N1, N2)
    let inline ( ./. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Division, N1, N2)

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let m = AR(3)
    let m2 = STAR(1,0.0,1.0,AR(1))
    let param = ARp([|0.6;-0.3;0.1|]) |> Sampling
    let param2 = STARp([|0.5|], [|-0.2|], 0.0,1.0,ARp([|0.5|])) |> Sampling
    //let smoother = WaveletSmoothing.HighFrequencyCut(1)
    //printfn "%A" (Graph.defaultStateForSampling (STARp([|0.7|], [|-0.2|], 0.0,1.0,ARp([|0.5|]))))

    //let sk = Node2(Multiplication,Leaf(Parameter(0)),Graph.defaultSkeletonForFitting m2)
(*    let sk = Graph.defaultSkeletonForFitting m2
    let gradientSk = SkeletonTree.gradientSkeletonForParameter 0 sk |> fst
    printfn "%A" (gradientSk)
    printfn "%A" (gradientSk |> SkeletonTree.simplify)
    DrawStructure.Model (sk |> SkeletonTree.simplify)*)

(*    let sk = Graph.defaultSkeletonForFitting m2
    DrawStructure.Model sk*)

    //let s = Models.GraphTS.sample 1000 param2
(*    let mean = s |> Array.fold (fun st x -> st + x / float s.Length) 0.0
    let std = s |> Array.fold (fun st x -> st + (x-mean)*(x-mean) / (float s.Length-1.0)) 0.0 |> sqrt
    let s = s |> Array.map (fun x -> x / std)*)
    //Chart.Line s |> Chart.Show

(*    let c1 = Chart.Line sd
    let s = s |> Array.scan (fun s x -> s + x) 0.0
    //printfn "%A" (WaveletSmoothing.SWT WaveletSmoothing.Haar s)
    let s = WaveletSmoothing.smooth WaveletSmoothing.Haar smoother s
    let s = s |> Array.windowed 2 |> Array.map (fun arr -> arr.[1] - arr.[0])
    Chart.Combine([c1; Chart.Line s]) |> Chart.Show*)
    
(*    let s = s |> Array.map (fun x -> Some x)
    let opti = Models.SGD.Adam(0.9,0.999,0.0005)
    let fittedModel, errors = Models.SGD.fit m opti 2000 s
    printfn "%A" fittedModel
    Chart.Line errors |> Chart.Show*)

(*    let arr = [|2;3;-4;5;9;-7;3;5;6;-7;3;4;-2;9;6;-1|] |> Array.map (fun x -> float x)
    let decomp = WaveletSmoothing.SWT WaveletSmoothing.Haar arr 
    //printfn "%A" (WaveletSmoothing.iSWT decomp WaveletSmoothing.Haar)
    let smoother = WaveletSmoothing.Soft(1.0)
    printfn "%A" (WaveletSmoothing.smooth WaveletSmoothing.Haar smoother arr)*)

(*    let arrF = Array.copy arr |> Array.map (fun x -> complex x 0.0)
    Fourier.Forward(arrF, FourierOptions.InverseExponent)
    printfn "%A" (arrF |> Array.map (fun x -> Complex.realPart x))*)
    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code