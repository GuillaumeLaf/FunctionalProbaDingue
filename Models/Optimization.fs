namespace Models

module Optimization = 
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra

    type ContinuousObjectiveFunction = 
        | LeastSquares of (float array -> float)

    type ObjectiveFunction = 
        | ContinuousFunction of ContinuousObjectiveFunction

    type ContinuousOptimizationMethod = 
        | BFGS

    type OptimizationMethod = 
        | ContinuousMethod of ContinuousOptimizationMethod

    type OptimizationProblem = OptimizationProblem of OptimizationMethod * ObjectiveFunction

    let run (array:float array) (T(name,graph,updateStrat)) = 
        


    

