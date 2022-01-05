module Distributions

    open System
    open MathNet.Numerics.Distributions
    // open DotNumerics.Optimization
    open PSO

    type T = 
        | Norm of mean : float * std : float

    let create (dist:T) : T option = 
        match dist with
        | Norm (mean, std) as x -> if std <= 0.0 then None else Some x

    let toParameterArray (dist:T) = 
        match dist with
        | Norm (mean, std) -> [|mean; std|]

    let getInitialGuess (dist:T) = 
        match dist with
        | Norm(mean, std) -> [|0.0; 1.0|]
    
    let sample (dist : T option) = 
        let func (d : T) = 
            match d with
            | Norm (mean, std) -> Normal.Sample(mean, std)
        dist |> Option.map func |> Option.defaultValue 0.0

    let getRandomList (n : int) (distr : T option) : float list = [ for i in 1..n -> sample distr ] 
    let getRandomArray (n : int) (distr : T option) : float array = [| for i in 1..n -> sample distr |] 

    // let pdf (dist:T) = 
    //     match dist with
    //     | Norm(mean, std) -> (fun x -> (1.0 / (std * sqrt (2.0 * Math.PI))) * exp (-1.0/2.0 *((x - mean)/std)*((x - mean)/std)))

    let logLikelihood (dist : T option) (p:float array) = 
        let func (p:float array) (d:T) = 
            match d with
            | Norm(mean, std) -> (fun x -> log(p.[1]) + 0.5 * ((x - p.[0]) / p.[1])*((x - p.[0]) / p.[1]))
        dist |> Option.map (func p) |> Option.defaultValue (fun  x -> - System.Double.MaxValue)
        // | Norm(mean, std) as d -> (fun x ->  - log (pdf d x))

    let totalLogLikelihood (dist:T option) (array:float array) (p:float array) = 
        match dist with
        | None -> - System.Double.MaxValue
        | Some d as x -> array |> Array.map (logLikelihood x p) |> Array.reduce ( + )

    // let exclude (idx:int) (x:float array) = 
    //     x
    //     |> Array.splitAt idx
    //     |> (fun sp -> Array.concat (seq {fst sp; (snd sp) |> Array.splitAt 1 |> snd}))

    // let deriv f (dx:float) (x0:float)  = 
    //     let x1 = x0 - dx
    //     let x2 = x0 + dx
    //     let y1 = f x1
    //     let y2 = f x2
    //     (y2 - y1) / (x2 - x1)

    // let partialParamFunction (f:(float array -> float)) (other_params:float array) (idx:int) (final_param:float) = 
    //     other_params 
    //         |> Array.splitAt idx
    //         |> (fun oP -> f (Array.concat (seq {fst oP; [|final_param|]; snd oP})))
    
    // let jacobian f (dx:float) (x: float array) = 
    //     x
    //     |> Array.mapi (fun i x0 -> deriv (partialParamFunction f (exclude i x) i) dx x0)

    // let BananaFunction (x: float array) =
    //     100.0 * Math.Pow((x.[1] - x.[0] * x.[0]), 2.0) + Math.Pow((1.0 - x.[0]), 2.0)

(*    let MLE (dist:T option) (array:float array) = 
        let likeli = totalLogLikelihood dist array
        let b = PSO.initializeBounds [|[|-5.0; 5.0|];[|0.0; 5.0|]|]
        let nIter = 100
        let optiSettings = {bounds=b; ndim=2; c1=1.5; c2=2.0; inertia=PSO.Linear (0.1, 1.2, nIter); nParticles=20; nIter=nIter}
        likeli |> PSO.minimize optiSettings*)
        // let initialGuess = getInitialGuess dist
        // let simplex = Simplex();
        // simplex.ComputeMin(likeli, initialGuess);
            // |> fromParameterArray n 
        

    


