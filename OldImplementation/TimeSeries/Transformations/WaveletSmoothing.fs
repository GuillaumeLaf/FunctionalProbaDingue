namespace Transformations

open MathNet.Numerics
open MathNet.Numerics.IntegralTransforms

module WaveletSmoothing = 
    type Wavelet = 
        | Haar
        | DB2

    let scalingFilter = function
        | Haar -> [|1.0/sqrt(2.0);1.0/sqrt(2.0)|]
        | DB2 -> [|0.48296291314453; 0.83651630373780; 0.22414386804201; -0.12940952255126|]

    let highPassFrom lowPass = 
        lowPass |> Array.rev
                |> Array.mapi (fun idx x -> if idx % 2 = 0 then (-1.0)*x else x)

    let waveletFilter = scalingFilter >> highPassFrom
    
    let reconstructionFilterFrom = Array.rev 
    let scalingReconstructionFilter = scalingFilter >> reconstructionFilterFrom
    let waveletReconstructionFilter = waveletFilter >> reconstructionFilterFrom

    let upsample (array:'T[]) =
        let outArray = Array.zeroCreate (array.Length * 2)
        outArray |> Array.iteri (fun idx x -> if idx % 2 = 0 then outArray.[idx] <- array.[idx/2])
        outArray

    let downSample array = (Array.indexed >> Array.choose (fun (idx,x) -> if idx % 2 = 0 then Some(x) else None)) array
        
    let upsampleNtimes n = Array.replicate n upsample |> Array.fold (fun f s -> s >> f) id

    // Implementation could be made faster by successive convolution with products of FFT. 
    let SWT wavelet signal = 
        let maxLevel = 5
        let lowPass = scalingFilter wavelet
        let highPass = waveletFilter wavelet

        // contains the details coefficients for each level (concat. approx. at the end)
        let decomp = Array.zeroCreate (maxLevel+1) |> Array.map (fun x -> Array.zeroCreate 1)

        let rec SWTatLevel lowPass highPass signal = function
            | 0 -> decomp.[decomp.Length-1] <- signal
            | i -> let currentLevel = maxLevel - i
                   decomp.[currentLevel] <- Utilities.convolution signal highPass |> Utilities.roll (-(2.0**(float currentLevel)) |> int)
                   let approxCoeffs = Utilities.convolution signal lowPass |> Utilities.roll (-(2.0**(float currentLevel)) |> int)
                   SWTatLevel (upsample lowPass) (upsample highPass) approxCoeffs (i-1)
        SWTatLevel lowPass highPass signal maxLevel
        decomp

    let iSWT wavelet (decomp:float[][]) = 
        let maxLevel = decomp.Length - 1 // '-1' since the last is the final approx. coeffs. 
        let reconLowPass = scalingReconstructionFilter wavelet |> upsampleNtimes (maxLevel-1)
        let reconHighPass = waveletReconstructionFilter wavelet |> upsampleNtimes (maxLevel-1)
        let details = decomp.[0..maxLevel-1]

        let folder detail (approx,reconLow,reconHigh) = 
            let reconLowPass = Utilities.matchSizeOf approx reconLow |> snd 
            let reconHighPass = Utilities.matchSizeOf approx reconHigh |> snd
            let convoLow = Utilities.convolution approx reconLowPass
            let convoHigh = Utilities.convolution detail reconHighPass
            let nextApprox = Array.map2 (fun x1 x2 -> (x1+x2)/2.0) convoLow convoHigh
            (nextApprox, downSample reconLow, downSample reconHigh)

        let out, _, _ = Array.foldBack folder details (decomp.[maxLevel],reconLowPass,reconHighPass)
        out 
    
    type Smoother = 
        | HighFrequencyCut of level:int // Nullify coefficients below level
        | Hard of threshold:float
        | Soft of threshold:float

    let nullifyHighFrequency level (decomp:float[][]) = 
        let zeros = Array.zeroCreate decomp.[0].Length
        Array.concat [|Array.replicate level zeros;decomp.[level..decomp.Length-1]|] 

    let applyHardThreshold threshold = 
        let thresholdLevel = Array.Parallel.map (fun x -> if abs x <= threshold then 0.0 else x)
        let thresholdLevel = Array.Parallel.map (fun x -> if abs x <= threshold then 0.0 else x)
        Array.map (fun x -> thresholdLevel x)

    let applySoftThreshold threshold = 
        let thresholdLevel = Array.Parallel.map (function | x when abs x <= threshold -> 0.0
                                                          | x when x > threshold -> x - threshold
                                                          | x when x < -threshold -> x + threshold
                                                          | x -> x)
        Array.map (fun x -> thresholdLevel x)

    let smooth wavelet = function
        | HighFrequencyCut(l) -> (SWT wavelet >> nullifyHighFrequency l >> iSWT wavelet)
        | Hard(thresh) -> (SWT wavelet >> applyHardThreshold thresh >> iSWT wavelet)
        | Soft(thresh) -> (SWT wavelet >> applySoftThreshold thresh >> iSWT wavelet)

        
        
