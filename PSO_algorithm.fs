module PSO

// Transform bounds of type 'float' to 'float32'. The initial function recieved will in general be of type 'float'.
// Transform the initial function recieved (float [] -> float) into (float32[] -> float32). The final result must then be again converted into 'float'.

open System
open System.Numerics
open UtilitiesSIMD

type Swarm = {
    positions : float32 array     // array of size nParticles * ndim
    velocities : float32 array     // array of size nParticles * ndim
    pBests : float32 array * float32 array
    gBest : float32 array * float32
} 
    
type Space = {
    ObjectiveFunc : (float32 array -> float32)
    ndim : int
    bounds : float32 array array;    // array of size nParticles * ndim
}

type Inertia =
    | Constant of float32
    | Linear of min:float32 * max:float32 * maxTime:int
    | Exponential of max:float32 * alpha:float32 * maxTime:int

type Settings = {
    bounds : float32 array array;    // 1st index : particle | 2nd index : coordinates
    ndim : int;
    c1 : float32;
    c2 : float32;
    inertia : Inertia;
    nParticles : int;
    nIter : int;
}

let r = Random()
let randomFloatWithin (l:float32) (u:float32) = float32(r.NextDouble()) * (u - l) + l
let randomArrayWithin (l:float32) (u:float32) (n:int) = [|for _ in 1..n -> randomFloatWithin l u|]
    
let initializeSpace (f:(float32 array -> float32)) (ndim:int) (b:float32 array array) : Space = 
    if (ndim <> b.Length) then
        raise (System.ArgumentException("'ndim' must be equal to 'b' length!"))
    else
        {ObjectiveFunc=f; ndim=ndim; bounds=b}

let boundsCenter (s:Space) = 
    Array.init (s.bounds.Length) (fun i -> 0.5f*(s.bounds.[i].[0] + s.bounds.[i].[1]))

let initializeSwarm (s:Space) (nParticles:int) = 
    let positions =  Array.init (nParticles*s.ndim) (fun i -> randomFloatWithin s.bounds.[i%s.ndim].[0] s.bounds.[i%s.ndim].[1])
    let velocities = Array.init (nParticles*s.ndim) (fun i -> randomFloatWithin (-abs(s.bounds.[i%s.ndim].[0] - s.bounds.[i%s.ndim].[1])) (abs(s.bounds.[i%s.ndim].[0] - s.bounds.[i%s.ndim].[1])))
    let pBestValues = Array.init nParticles (fun _ -> Single.MaxValue)
    let pBests = (positions , pBestValues)
    let gBest = (boundsCenter s, Single.MaxValue)
    {positions=positions; velocities=velocities; pBests=pBests; gBest=gBest}
    
let clipToBound (b:float32 array) = function 
    | x when (x < b.[0]) -> b.[0] + 0.0000000001f
    | x when (x > b.[1]) -> b.[1] - 0.0000000001f
    | x -> x
    
let updatePositions (s:Space) ({positions=positions; velocities=velocities}) = 
    positions |> ArraySIMD.add velocities
              |> Array.mapi (fun i x -> clipToBound s.bounds.[i%s.ndim] x)
    
let InertiaFormula = function
    | Constant (w) -> (fun t -> w)
    | Linear (min, max, maxTime) -> (fun t -> max - (max - min) * (float32(t) / float32(maxTime)))
    | Exponential (max, alpha, maxTime) -> (fun t -> max * exp (- alpha * (float32(t) / float32(maxTime))*(float32(t) / float32(maxTime))))

let limitVelocity (maxVelocity:float32) = function
    | x when (x < -maxVelocity) -> -maxVelocity
    | x when (x > maxVelocity) -> maxVelocity
    | x -> x
    
let updateVelocities (s:Space) (inertia:Inertia) (time:int) (c1:float32) (c2:float32) ({positions=positions; velocities=velocities; pBests=pBests; gBest=gBest}) =
    // let VelocityFormula v0 p0 pB0 gB0 = inertiaValue * v0 + c1 * (randomFloatWithin 0.0f 1.0f) * (pB0 - p0) + c2 * (randomFloatWithin 0.0f 1.0f) * (gB0 - p0)
    let len = (snd pBests).Length
    let maxVelocity = [| for i in 0..s.ndim-1 do abs(s.bounds.[i].[1]-s.bounds.[i].[0])*0.3819f|]
    let inertiaValue  = time |> InertiaFormula inertia
    let inertiaConservation = velocities |> ArraySIMD.multScalar inertiaValue

    let personalAttraction = positions |> ArraySIMD.substract (fst pBests)
                                        |> ArraySIMD.mult (randomArrayWithin 0.0f 1.0f positions.Length)
                                        |> ArraySIMD.multScalar c1
                                         
    let globalAttraction = positions |> ArraySIMD.substract (ArraySIMD.repeat len (fst gBest))
                                     |> ArraySIMD.mult (randomArrayWithin 0.0f 1.0f positions.Length)
                                     |> ArraySIMD.multScalar c2
                                   
    inertiaConservation |> ArraySIMD.add personalAttraction
                        |> ArraySIMD.add globalAttraction
                        |> Array.mapi (fun i x -> x |> limitVelocity maxVelocity.[i%s.ndim])

let updatePBests (s:Space) ({positions=positions; velocities=velocities; pBests=pBests; gBest=gBest}) = 
    let newPBests = (Array.copy (fst pBests), Array.copy (snd pBests))
    let newFuncValues = Array.zeroCreate (snd pBests).Length
                            |> Array.Parallel.mapi (fun i _ -> s.ObjectiveFunc positions.[i*s.ndim..i*s.ndim + (s.ndim-1)])
                            |> Array.mapi (fun i x -> if x < (snd pBests).[i] then  
                                                        (fst newPBests).[i*s.ndim..i*s.ndim + (s.ndim-1)] <- positions.[i*s.ndim..i*s.ndim + (s.ndim-1)]
                                                        (snd newPBests).[i] <- x)
    newPBests

let updateGlobalBest (s:Space) ({pBests=pBests; gBest=gBest}) = 
    let minIdx = pBests |> snd |> Array.indexed |> Array.minBy snd |> fst
    if (snd pBests).[minIdx] < (snd gBest) then
        ((fst pBests).[minIdx*s.ndim..minIdx*s.ndim+(s.ndim-1)], (snd pBests).[minIdx])
    else gBest

let minimize ({bounds=bounds; ndim=ndim; c1=c1; c2=c2; inertia=inertia; nParticles=nParticles; nIter=nIter}) (func:(float array -> float)) =
    let downCastFunc (x:float32 array) = x |> Array.map float |> func |> float32
    let space = initializeSpace downCastFunc ndim bounds
    let swarm = initializeSwarm space nParticles
    let updateSwarm (s:Swarm) (time:int) = 
            let newPBests = updatePBests space s
            {positions=(updatePositions space s); velocities=(updateVelocities space inertia time c1 c2 s); pBests=newPBests; gBest=(updateGlobalBest space s)}

    let rec loop acc i = 
        match i with
        | 0 -> acc.gBest
        | _ -> //printfn "Solution n° %i : %A" (nIter-i) acc.gBest
               loop (updateSwarm acc (nIter - i)) (i-1)
    loop swarm nIter
    
