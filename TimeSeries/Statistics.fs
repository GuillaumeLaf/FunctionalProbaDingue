namespace TimeSeries

open Monads

module Statistics = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>=>) g f = Monad.compose g f

    // Sum the data in the TimeSeries monad.
    // 'f' is a function applied to each element before summing.
    let sumM f = Some <!> ((Array.fold (fun s x -> Option.fold (fun s x -> s + f x) s x) 0.0) <!> Univariate.dataM)

    let meanM = 
        (fun length -> Option.map (fun sum -> sum / length))
            <!> Univariate.lengthM () <*> sumM id
    
    let stdM = 
        (fun length -> Option.map2 (fun sumSquared mean -> sumSquared / length - (mean*mean) |> sqrt))
            <!> Univariate.lengthM () <*> sumM (fun x -> x*x) <*> meanM

