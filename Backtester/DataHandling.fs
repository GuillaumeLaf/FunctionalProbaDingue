namespace Backtester

open System

module DataTransformations = // must output an array of same length as the given one.

    let mapDiff f (array: float array) = 
        let len = array.Length
        let result = Array.zeroCreate len // last element will always be 0.0
        let mutable i = 1
        while i < len do
            result.[i-1] <- f (array.[i]) - f (array.[i-1])
            i <- i + 1
        result
(*
    let takeLogReturn array = mapDiff log array
    let takeDifference array = mapDiff id array*)

module Data = 

    let roll (arr:_[]) (target:_[]) = Array.blit arr 0 target 1 (arr.Length - 1)
    
    (*let fromArray array = 
        let logReturns = array |> DataTransformations.takeDifference
        let transfoData = Array.zeroCreate array.Length
        transfoData |> roll logReturns;
        array |> Array.mapi (fun i x -> {time=i;price=x;logReturn=logReturns.[i];transformedData=transfoData.[i]})*)
             
        
        


