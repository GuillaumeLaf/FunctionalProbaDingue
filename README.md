# FunctionalProbaDingue
New Version of ProbaDingue but this time using F#.

# About The Project
This Project constitutes my first real attempt at making a viable algorithmic trading system. I am currently in my last year of studies in Econometric at the UCLouvain (Louvain-la-Neuve). I believe that clear understanding of models and system is key to be trading profitably in the long run. Consequently, I implement my trading system using well-known and studied econometric models. This project is also the time to learn new concepts and apply them empirically. I originally wanted to create a brand new model to understand the markets but I soon realized that all my "findings" were already well understood and completely generalized. For instance, I was interested in creating an adaptative model based on dummy variables which would create non-linearities in the parameters but I remember that "Markov-switching models" were a thing... This project will thus implement a majority of those well-crafted models using various techniques. It was also the opportunity to code using *Functional programming* paradigm.
## Major Techniques
The major techniques I use are listed below. I already had an idea about most of the techniques but I really had to dig deeper in order to code them. 
+ Functional programming
  + Monad
  + Recursion
  + Lambda Calculus
  + Monoïd
+ Computational Graphs
+ Optimization (global - local) 
  + Particle Swarm Optimization 
  + BFGS (with MathNet.Numerics) 
+ Models
  + AR / MA
  + SETAR
+ SIMD instructions
+ Parallelization

# TO DO
- [ ] Parallel Tree traversal
+ Optimization techniques
  - [ ] Maximum Likelihood
  - [ ] Integer/Mixed Optimization   
  - [ ] Further PSO Algorithm
- [ ] Forecast Confidence Interval (Exact - Bootstrap)
+ Models
  - [ ] Marko-Switching Models
  - [ ] Smooth-Transition Models
  - [ ] ARCH - GARCH
+ Statistical Testing
  - [ ] (Non)Linearity
  - [ ] (In)dependence
  - [ ] Heteroscedasticity
  - [ ] Normally Distributed
  - [ ] (Non)Stationarity
- [ ] Rewrite using float32 (faster computations).
