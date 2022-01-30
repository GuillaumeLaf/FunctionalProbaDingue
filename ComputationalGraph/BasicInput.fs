namespace ComputationalGraph

open GraphType

module BasicInput =
    
    let increment (input:BasicInput) (n:int) = 
        match input with 
        | Parameter(grpidx,idx) -> Parameter(grpidx,idx+n)
        | Variable(grpidx,idx) -> Variable(grpidx,idx+n)
        | Innovation(grpidx,idx) -> Innovation(grpidx,idx+n)
    
    let comparison grpidx1 grpidx2 idx1 idx2 = 
        match grpidx1,grpidx2,idx1,idx2 with
        | _ when (grpidx1=grpidx2) && (idx1=idx2) -> 0
        | _ when (grpidx1=grpidx2) && (idx1<idx2) -> -1
        | _ when (grpidx1=grpidx2) && (idx1>idx2) -> 1
        | _ when (grpidx1<grpidx2) -> -1
        | _ when (grpidx1>grpidx2) -> 1
        | _ -> 0    // !!!

    let compare (input1:BasicInput) (input2:BasicInput) = 
        match (input1,input2) with
        | Parameter(grpidx1,idx1),Parameter(grpidx2,idx2) -> comparison grpidx1 grpidx2 idx1 idx2
        | Variable(grpidx1,idx1),Variable(grpidx2,idx2) -> comparison grpidx1 grpidx2 idx1 idx2
        | Innovation(grpidx1,idx1),Innovation(grpidx2,idx2) -> comparison grpidx1 grpidx2 idx1 idx2
        | _ -> invalidArg "input1-input2" "Comparison not available between those type of 'BasicInput'."