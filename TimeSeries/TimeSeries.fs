namespace TimeSeries

open Monads

module Univariate = 
    type State<'T> = State of int * 'T option [] * innovations:'T option []  // Option type to handle missing data

    let defaultState n = State(0, Array.init n (fun _ -> Some 0.0), Array.init n (fun _ -> Some 0.0))
    let defaultStateFrom array = 
        State(0, array, Array.init array.Length (fun _ -> Some 0.0))

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>=>) g f = Monad.compose g f

    let stepM = 
        let innerFunc (State(idx,data,innovations)) = (), (State(idx+1,data,innovations))
        Monad.M innerFunc

    // The stepping monad must be last since it updates the state index.
    let stepping m = (fun m _ -> m) <!> m <*> stepM

    let StateM = 
        let innerFunc (State(idx,data,innovations)) = (State(idx,data,innovations)), (State(idx,data,innovations))
        Monad.M innerFunc

    let dataM = 
        let innerFunc (State(idx,data,innovations)) = data, (State(idx,data,innovations))
        Monad.M innerFunc

    let idxM = 
        let innerFunc (State(idx,data,innovations)) = idx, (State(idx,data,innovations))
        Monad.M innerFunc

    let innovationsM = 
        let innerFunc (State(idx,data,innovations)) = innovations, (State(idx,data,innovations))
        Monad.M innerFunc

    let setStateM state = 
        let innerFunc oldState = state, state
        Monad.M innerFunc

    let setDataM data = 
        let innerFunc (State(idx,_,innovations)) = data, (State(idx,data,innovations))
        Monad.M innerFunc

    let stateKeeping m = fst <!> (Monad.run (m >>= (fun result -> setStateM <!> StateM >>= (fun _ -> Monad.rets result))) <!> StateM)
                                       
    let dataUpdating m = m >>= (fun x -> setDataM x)

    let lengthM () = float <!> (Array.length <!> dataM)

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

    let currentElementM () = Array.get <!> dataM <*> idxM
    let currentInnovationM () = Array.get <!> innovationsM <*> idxM

    let chooseElementAtLag lag idx (array:'a option[]) = 
        match (idx-lag) with
        | x when x < 0 -> None
        | x when x >= array.Length -> None
        | x -> array.[x]

    let elementAtLagM lag = (chooseElementAtLag lag) <!> idxM <*> dataM
    let innovationAtLagM lag = (chooseElementAtLag lag) <!> idxM <*> innovationsM

    let elementAtLeadM lead = elementAtLagM (-lead)
    let innovationAtLeadM lead = innovationAtLagM (-lead)

    let previousElementsM maxLag = 
        [1..maxLag] |> Monad.traverse elementAtLagM
                    |> Monad.map (Array.ofList)

    // This map function doesn't update the state. 
    let mapM m = fst <!> (Array.mapFold (fun s _ -> Monad.run (stepping m) s) <!> StateM <*> dataM)

    // This map function extends the previous one by updating the state with the result.
    let mapReplaceM m = dataUpdating (mapM m)
        

(*module Multivariate = 
    type States<'T> = States of Univariate.State<'T>[]  

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
    
    let (stepM:Monad.M<States<float>,unit[]>) = Univariate.stepM |> mapOverUnivariateM
    let (StateM:Monad.M<States<float>,float option[][]>) = Univariate.StateM |> mapOverUnivariateM
    let (currentElementM:Monad.M<States<float>,float option[]>) = Univariate.currentElementM |> mapOverUnivariateM
    let elementAtLagM lag = (Univariate.elementAtLagM lag) |> mapOverUnivariateM
    let innovationAtLagM lag = (Univariate.innovationAtLagM lag) |> mapOverUnivariateM
    let elementAtLeadM lead = (Univariate.elementAtLeadM lead) |> mapOverUnivariateM
    let innovationAtLeadM lead = (Univariate.innovationAtLeadM lead) |> mapOverUnivariateM
    let differencedM f = (Univariate.differencedM f) |> mapOverUnivariateM

    let stepping m = 
        let tmpFunc m step = m
        tmpFunc <!> m
                <*> stepM*)
            



