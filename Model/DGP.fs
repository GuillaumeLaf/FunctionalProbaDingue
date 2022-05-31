namespace Models

module DGP = 
    open ModelType

    module VAR = 
         // Convert parameters from shape in GraphState to correct shape in 'VAR'.
         let convertParameters (p:'T[,]) (var:VAR<'T>) = Array.init var.order (fun i -> p.[*,(var.n*i)..(var.n*(i+1)-1)])
         let setParameters (p:'T[,]) (var:VAR<'T>) = { var with parameters= convertParameters p var |> Some }



    let rec setParameters p = function
        | VAR(var) -> VAR.setParameters p var |> VAR
        | ErrorModel(inner,errType) -> (setParameters p inner, errType) |> ErrorModel