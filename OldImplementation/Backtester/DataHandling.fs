namespace Backtester

open System

module Data = 

    let roll (arr:_[]) (target:_[]) = Array.blit arr 0 target 1 (arr.Length - 1)
    
    (*let fromArray array = 
        let logReturns = array |> DataTransformations.takeDifference
        let transfoData = Array.zeroCreate array.Length
        transfoData |> roll logReturns;
        array |> Array.mapi (fun i x -> {time=i;price=x;logReturn=logReturns.[i];transformedData=transfoData.[i]})*)
             
        
        


