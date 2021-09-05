namespace Model

module MonadicGraph = 
    type Op = 
        | Addition
        | Multiplication

    type Input<'T> = 
        | Parameter of idx:int 
        | Variable of idx:int
        | Innovation of idx:int
        | Constant of value:'T

    type Skeleton<'T> = 
        | Leaf of Input<'T> 
        | Node of Op * Skeleton<'T> * Skeleton<'T>
    
    type State<'T> = State of parameters:'T[] * variables:'T[]

    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Multiplication, N1, N2)

    let fixedParameterM idx = 
        let innerFunc (State(p,v)) = p.[idx], (State(p,v))
        Monad.M innerFunc

    let fixedVariableM idx = 
        let innerFunc (State(p,v)) = v.[idx], (State(p,v))
        Monad.M innerFunc

    let inline fold nodeF leafV sk = 
        let rec loop n k = 
            match n with
                | Node(op,left,right) -> nodeF op (loop left) (loop right) n k
                | Leaf(input) -> leafV input n k
        loop sk id

    let inline skeletonM parameterM variableM innovationM skeleton = 
        fold (fun op kl kr _ k -> match op with
                                    | Addition -> kl (fun lacc -> kr (fun racc -> (Monad.add lacc racc) |> k)) 
                                    | Multiplication -> kl (fun lacc -> kr (fun racc ->  (Monad.mult lacc racc) |> k))
                                    )
             (fun input _ k -> match input with
                                | Parameter(idx) -> parameterM idx |> k
                                | Variable(idx) -> variableM idx |> k
                                | Innovation(idx) -> innovationM idx |> k  
                                | Constant(value) -> Monad.rets value |> k)
             skeleton 

    type Model = 
        | AR of order:int
        | MA of order:int
        | SETAR of order:int * delay:int // The two AR models have the same order

(*    let defaultState = function
        | AR(order) -> State(Array.zeroCreate order, Array.zeroCreate order)
        | MA(order) -> State(Array.zeroCreate order, Array.zeroCreate order)
        | SETAR(order,delay) -> State(Array.zeroCreate (2*order+1), Array.zeroCreate (order+1))*)

    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec variableUpdate = function
        | AR(order) -> [| for i in 1..order do TimeSeries.UnivariateTimeSeries.elementAtLagM i |]
        | MA(order) -> [| for i in 1..order do TimeSeries.UnivariateTimeSeries.innovationAtLagM i |]
        | SETAR(order,delay) -> Array.concat [|variableUpdate (AR(order)); [|TimeSeries.UnivariateTimeSeries.elementAtLagM delay|]|]

(*    let updateVariableM m = 
        let innerFunc tsState = 
            (variableUpdate m) |> Array.map (fun x -> Monad.run x tsState |> fst), tsState
        Monad.M innerFunc*)

    let sampleModelTimeSeriesM m skM (State(p,v)) = 
        let innerFunc (TimeSeries.UnivariateTimeSeries.State(idx,data,innovation)) =
            let result, graphState = Monad.run skM (State(p,v))
            data.[idx] <- result
            let newVariables = (variableUpdate m) |> Array.map (fun x -> Monad.run x (TimeSeries.UnivariateTimeSeries.State(idx,data,innovation)) |> fst |> Option.defaultValue 0.0)
            (State(p,newVariables)), (TimeSeries.UnivariateTimeSeries.State(idx,data,innovation))
        Monad.M innerFunc



// The final Monad must be a Time Series Monad which gives me either its state or the Monad graph (I don't know yet which). 
// The latter contains either the simulated TS or the original TS with error of the model.

(*    let modelM model skM (State(p,v)) =
        let innerFunc (TimeSeries.UnivariateTimeSeries.State(idx,data,innovation)) = 
            // here execute the loop through the TS state
            // Then repack the whole thing into a TS Monad.*)




            

            
            


    let ma1:Skeleton<float> = Leaf(Parameter(0)) .*. Leaf(Variable(0)) .+. Leaf(Constant(1.0))