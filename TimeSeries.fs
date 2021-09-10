namespace TimeSeries

open Monads

module UnivariateTimeSeries = 
    type State<'T> = State of int * 'T option [] * innovations:'T option []  // Option type to handle missing data

    let defaultState n = State(0, Array.init n (fun _ -> Some 0.0), Array.init n (fun _ -> Some 0.0))
    let defaultStateFrom array = 
        State(0, array, Array.init array.Length (fun _ -> Some 0.0))

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let stepM = 
        let innerFunc (State(idx,data,innovations)) = (), (State(idx+1,data,innovations))
        Monad.M innerFunc

    let stepping m = 
        let tmpFunc m step = m
        tmpFunc <!> m
                <*> stepM

    let StateM = 
        let innerFunc (State(idx,data,innovations)) = data, (State(idx,data,innovations))
        Monad.M innerFunc

    let setCurrentElementM x = 
        let innerFunc (State(idx,data,innovations)) = 
            data.[idx] <- Some x
            (), (State(idx,data,innovations))
        Monad.M innerFunc

    let setCurrentInnovationM x = 
        let innerFunc (State(idx,data,innovations)) = 
            innovations.[idx] <- Some x
            (), (State(idx,data,innovations))
        Monad.M innerFunc

    let currentElementM = 
        let innerFunc (State(idx,data,innovations)) = data.[idx], (State(idx,data,innovations))
        Monad.M innerFunc

    let currentInnovationM = 
        let innerFunc (State(idx,data,innovations)) = innovations.[idx], (State(idx,data,innovations))
        Monad.M innerFunc

    let elementAtLagM lag = 
        let innerFunc (State(idx,data,innovations)) = 
            if idx-lag < 0 then
                None, (State(idx,data,innovations))
            else
                data.[idx-lag], (State(idx,data,innovations))
        Monad.M innerFunc

    let innovationAtLagM lag = 
        let innerFunc (State(idx,data,innovations)) = 
            if idx-lag < 0 then
                None, (State(idx,data,innovations))
            else
                innovations.[idx-lag], (State(idx,data,innovations))
        Monad.M innerFunc

    let elementAtLeadM lead = 
        let innerFunc (State(idx,data,innovations)) = 
            if (idx+lead) >= data.Length then
                None, (State(idx,data,innovations))
            else 
                data.[idx+lead], (State(idx,data,innovations))
        Monad.M innerFunc

    let innovationAtLeadM lead = 
        let innerFunc (State(idx,data,innovations)) = 
            if (idx+lead) >= data.Length then
                None, (State(idx,data,innovations))
            else 
                innovations.[idx+lead], (State(idx,data,innovations))
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
        let innerFunc (State(idx,data,innovations)) = 
            let sequenceM = data |> Array.skip idx
                                 |> Array.map ( fun _ -> stepping m )
                                 |> Array.toList
                                 |> Monad.sequence
            let result = Monad.run sequenceM (State(idx,data,innovations)) |> fst |> Array.ofList
            result, (State(idx,result,innovations))
        Monad.M innerFunc

    let differencedSeriesM = mapM (differencedM id)
    let logDifferencedSeriesM = mapM (differencedM log)
        
module MultivariateTimeSeries = 
    type States<'T> = States of UnivariateTimeSeries.State<'T>[]  

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let mapTuple f1 f2 (x1,x2) = f1 x1, f2 x2

    let inline mapOverStates m (States(s)) = 
        s |> Array.mapFold (fun state x -> Monad.run m x |> mapTuple id (fun x -> x :: state)) []
          |> mapTuple id (Array.ofList >> States)

    let inline mapOverUnivariateM m = 
        let innerFunc state = 
            state |> mapOverStates m
        Monad.M innerFunc
    
    let (stepM:Monad.M<States<float>,unit[]>) = UnivariateTimeSeries.stepM |> mapOverUnivariateM
    let (StateM:Monad.M<States<float>,float option[][]>) = UnivariateTimeSeries.StateM |> mapOverUnivariateM
    let (currentElementM:Monad.M<States<float>,float option[]>) = UnivariateTimeSeries.currentElementM |> mapOverUnivariateM
    let elementAtLagM lag = (UnivariateTimeSeries.elementAtLagM lag) |> mapOverUnivariateM
    let innovationAtLagM lag = (UnivariateTimeSeries.innovationAtLagM lag) |> mapOverUnivariateM
    let elementAtLeadM lead = (UnivariateTimeSeries.elementAtLeadM lead) |> mapOverUnivariateM
    let innovationAtLeadM lead = (UnivariateTimeSeries.innovationAtLeadM lead) |> mapOverUnivariateM
    let differencedM f = (UnivariateTimeSeries.differencedM f) |> mapOverUnivariateM

    let stepping m = 
        let tmpFunc m step = m
        tmpFunc <!> m
                <*> stepM
            



