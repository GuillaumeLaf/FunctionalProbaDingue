namespace TimeSeries

open Monads

module Transformations = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>=>) g f = Monad.compose g f
    let (>=<) g f = Monad.chain g f

    let inline differencedM (f:'a->'a) = 
        Option.map2 (fun current previous -> f current - f previous) 
                <!> Univariate.currentElementM ()
                <*> Univariate.elementAtLagM 1

    let differencedSeriesM = Univariate.mapM (differencedM id)
    let logDifferencedSeriesM = Univariate.mapM (differencedM log)

    let demeanM = 
        Monad.state{
            let! mean = Statistics.meanM
            do! Univariate.addTransformationM (Univariate.Mean(mean))
            return! Monad.state{
                        let! current = Univariate.currentElementM()
                        return Option.map2 (fun current mean -> current - mean) current mean
                    } |> Univariate.mapReplaceM // map over every element in 'data' and replace inital 'data'.
        }

    let inline inverseDemeanM mean = 
        Monad.state{
            do! Univariate.removeTransformationM (Univariate.Mean(mean))
            return! Monad.state {
                    let! current = Univariate.currentElementM()
                    return Option.map2 (fun current mean -> current + mean) current mean
                    } |> Univariate.mapReplaceM
        }
        

    let standardizeM = 
        Monad.state{
            let! std = Statistics.stdM
            do! Univariate.addTransformationM (Univariate.Std(std))
            return! Monad.state{
                        let! current = Univariate.currentElementM()
                        return Option.map2 (fun current std -> current / std) current std
                    } |> Univariate.mapReplaceM 
        }

    let inline inverseStandardizeM std =
        Monad.state{
            do! Univariate.removeTransformationM (Univariate.Std(std))
            return! Monad.state {
                        let! current = Univariate.currentElementM()
                        return Option.map2 (fun current std -> current * std) current std
                    } |> Univariate.mapReplaceM
        }
        

    // 'demeanM' must modify the state with the result of its computations,
    // if we then want to standardize the variance.
    // We finally comeback to the initial state by making it 'stateKeeping'.
    let normalizeM = (demeanM >=< standardizeM)

    let inline _inverseFrom transformation = 
        match transformation with
        | Univariate.Mean(mean) -> inverseDemeanM mean
        | Univariate.Std(std) -> inverseStandardizeM std

    let inline inverseTransformationsM() = 
        Monad.state {
            let! transListM = List.map (fun t -> _inverseFrom t) <!> Univariate.transformationsM
            return! List.fold (>=<) (List.head transListM) (List.tail transListM)
        }

                             
    