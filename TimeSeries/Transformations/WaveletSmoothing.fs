namespace Transformations

open MathNet.Numerics
open MathNet.Numerics.IntegralTransforms

module WaveletSmoothing = 
    type Wavelet = 
        | Haar

    let scalingFilter = function
        | Haar -> [|1.0/sqrt(2.0);1.0/sqrt(2.0)|]

    let highPassFrom lowPass = 
        lowPass |> Array.rev
                |> Array.mapi (fun idx x -> if idx % 2 = 0 then x else (-1.0)*x)

    let waveletFilter = scalingFilter >> highPassFrom

    let upsample array =
        0

    // Do I have to upsample the filters or compute the filters for each level with the cascade algorithm ?

    let SWT signal wavelet = 
        let maxLevel = 2
        let lowPass = scalingFilter wavelet
        let highPass = waveletFilter wavelet
        // contains the details coefficients for each level (concat. approx. at the end)
        let decomp = Array.zeroCreate maxLevel |> Array.map (fun x -> Array.zeroCreate 1)

        let rec SWTatLevel lowPass highPass signal = function
            | 0 -> ()
            | i -> let currentLevel = maxLevel - i
                   decomp.[currentLevel] <- Utilities.convolution signal highPass |> Utilities.roll -(2**currentLevel)
                   let approxCoeffs = Utilities.convolution signal lowPass |> Utilities.roll -(2**currentLevel)




            

        
        
