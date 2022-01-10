namespace TimeSeries

open FSharpPlus
open FSharpPlus.Data

module TimeSeries = 
    type S<'T> = S of 'T []

    let testM : State<S<float>,unit> = monad {
        return ()
    }

    let fold n = monad {
        let arrayM = Array.zeroCreate n |> Array.map (fun _ -> testM)
        let! s = State.get
        let _ = (s,arrayM) ||> Array.mapFold (fun s x -> State.run x s)
        return ()
    }
