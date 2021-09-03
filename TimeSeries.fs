module TimeSeries
    type State<'T> = State of int * 'T option []  // Option type to handle missing data

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let stepM = 
        let innerFunc (State(idx,data)) = (), (State(idx+1,data))
        Monad.M innerFunc

    let stepping m = 
        let tmpFunc m step = m
        tmpFunc <!> m
                <*> stepM

    let StateM = 
        let innerFunc (State(idx,data)) = data, (State(idx,data))
        Monad.M innerFunc

    let currentElementM = 
        let innerFunc (State(idx,data)) = data.[idx], (State(idx,data))
        Monad.M innerFunc

    let elementAtLagM lag = 
        let innerFunc (State(idx,data)) = 
            if idx-lag < 0 then
                None, (State(idx,data))
            else
                data.[idx-lag], (State(idx,data))
        Monad.M innerFunc

    let elementAtLeadM lead = 
        let innerFunc (State(idx,data)) = 
            if (idx+lead) >= data.Length then
                None, (State(idx,data))
            else 
                data.[idx+lead], (State(idx,data))
        Monad.M innerFunc

    let previousElementsM maxLag = 
        [1..maxLag] |> Monad.traverse elementAtLagM
                    |> Monad.map (Array.ofList)

    let differencedM f = 
        let innerFuncVanilla (current:float) (previous:float) = f current - f previous
        let innerFuncOption current previous = Option.map2 (fun c p -> innerFuncVanilla c p) current previous
        innerFuncOption <!> currentElementM
                        <*> elementAtLagM 1

    let mapM m = 
        let innerFunc (State(idx,data)) = 
            let sequenceM = data |> Array.skip idx
                                 |> Array.map ( fun _ -> stepping m )
                                 |> Array.toList
                                 |> Monad.sequence
            let result = Monad.run sequenceM (State(idx,data)) |> fst |> Array.ofList
            result, (State(idx,result))
        Monad.M innerFunc

    let differencedSeriesM = mapM (differencedM id)
    let logDifferencedSeriesM = mapM (differencedM log)
        
            



            
            



