module Monad

    type M<'State,'T> = M of ('State -> 'T * 'State)

    let run (M f) initialState = f initialState

    let map f m = 
        let innerFunc state = 
            let innerType, nextState = run m state
            (f innerType), nextState
        M innerFunc

    let rets x = 
        let innerFunc state = x, state
        M innerFunc

    let bind f m = 
        let innerFunc state = 
            let innerType, nextState = run m state
            run (f innerType) nextState
        M innerFunc

    let (>>=) x f = bind f x

    let apply mf mx = 
        let innerFunc state = 
            let f, nextState = run mf state
            let x, nextState2 = run mx nextState
            (f x), nextState2
        M innerFunc

    let traverse f list =  // Not tail-recursive for some reason.
        let cons hd tl = hd :: tl

        let initState = rets []
        let folder tail head =
            f head >>= (fun h ->
            tail >>= (fun t ->
            rets (cons h t) ))

        List.fold folder initState list  

    let sequence list = traverse id list

    let inline operation op m1 m2 = 
        m1 >>= (fun x1 ->
        m2 >>= (fun x2 ->
        rets (op x1 x2)))

    let inline add m1 m2 = operation (+) m1 m2
    let inline mult m1 m2 = operation ( * ) m1 m2