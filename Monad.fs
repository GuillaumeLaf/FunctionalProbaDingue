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

    let apply mf mx = 
        let innerFunc state = 
            let f, nextState = run mf state
            let x, nextState2 = run mx nextState
            (f x), nextState2
        M innerFunc

    let traverse f list =  // Not tail-recursive for some reason.
        let (>>=) x f = bind f x
        let cons hd tl = hd :: tl

        let initState = rets []
        let folder tail head =
            f head >>= (fun h ->
            tail >>= (fun t ->
            rets (cons h t) ))

        List.fold folder initState list  

    let sequence list = traverse id list

