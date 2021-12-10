namespace Models

module Nodes = 
    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Multiplication, N1, N2)
    let inline ( .-. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Substraction, N1, N2)
    let inline ( ./. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Division, N1, N2)

    let linearCombinaisons n = 
        Array.zeroCreate n |> Array.mapi (fun i _ -> Leaf(Parameter(i)) .*. Leaf(Variable(i)))
        |> Array.reduce (.+.)
        
    // The 1st node will determined the renumbering of the 2nd node.
    // The function with suffix "-ShiftF" receives the number of the corresponding node  
    // in the 1st tree and output the shift of the numbering for the 2nd node.
    // The current implementation does not allow multivariate mixingNode. 
    let mixture parameterShiftF variableShiftF innovationShiftF mixingNode node1 node2 = 
        let nLeaves1 = SkeletonTree.countLeaves node1
        let shiftedNode2 = SkeletonTree.shift (parameterShiftF nLeaves1.[0])
                                              (variableShiftF nLeaves1.[1])
                                              (innovationShiftF nLeaves1.[2])
                                              node2
        let nLeaves12 = SkeletonTree.countLeaves (node1 .+. node2)
        let shiftedMixingNode = SkeletonTree.shift (nLeaves12.[0]) 0 0 mixingNode
        (shiftedMixingNode .*. node1) .+. ((Leaf(Constant(1.0)) .-. shiftedMixingNode) .*. shiftedNode2)

    let logisticNode gamma c x = 
        let exponent = Leaf(Constant(-gamma)) .*. (x .-. Leaf(Constant(c)))
        Leaf(Constant(1.0)) ./. (Leaf(Constant(1.0)) .+. Node1(Exponential, exponent))
        
