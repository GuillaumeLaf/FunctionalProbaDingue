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

    let upsample (array:'T[]) =
        let outArray = Array.zeroCreate (array.Length * 2)
        outArray |> Array.iteri (fun idx x -> if idx % 2 = 0 then outArray.[idx] <- array.[idx/2])
        outArray

    let SWT signal wavelet = 
        let maxLevel = 2
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

    


            

        
        
