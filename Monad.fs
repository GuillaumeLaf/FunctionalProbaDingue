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

    let rec foldr folder list initState k = 
        match list with
        | [] -> k initState
        | x::xs -> foldr folder xs initState (fun r -> k (folder x r)) 

    let traverse f list =
        let (>>=) x f = bind f x
        let cons hd tl = hd :: tl

        let initState = rets []
        let folder head tail =
            f head >>= (fun h ->
            tail >>= (fun t ->
            rets (cons h t) ))

        List.foldBack folder list initState
        //foldr folder list initState id

    let sequence list = traverse id list