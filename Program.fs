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
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let m = AR(1)
    let param = ARp([|0.7|]) |> Sampling
    let smoother = WaveletSmoothing.HighFrequencyCut(1)

    let s = Models.GraphTS.sample 1000 param 
    let c1 = Chart.Line s
    let s = s |> Array.scan (fun s x -> s + x) 0.0
    //printfn "%A" (WaveletSmoothing.SWT WaveletSmoothing.Haar s)
    let s = WaveletSmoothing.smooth WaveletSmoothing.Haar smoother s
    let s = s |> Array.windowed 2 |> Array.map (fun arr -> arr.[1] - arr.[0])
    Chart.Combine([c1; Chart.Line s]) |> Chart.Show
    
    let s = s |> Array.map (fun x -> Some x)
    let opti = Models.SGD.RMSProp(0.9,[|0.0|],0.005)
    let fittedModel = Models.SGD.fit m opti 200 s
    printfn "%A" fittedModel

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