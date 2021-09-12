namespace Models

module Nodes = 
    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node(Multiplication, N1, N2)

    let linearCombinaisons n = 
        Array.zeroCreate n |> Array.mapi (fun i _ -> Leaf(Parameter(i)) .*. Leaf(Variable(i)))
        |> Array.reduce (.+.)
        

