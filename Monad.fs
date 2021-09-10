namespace Monads

module Monad =

    type M<'State,'T> = M of ('State -> 'T * 'State)

    let run (M f) initialState = f initialState

    let rets x = 
        let innerFunc state = x, state
        M innerFunc

    let bind f m = 
        let innerFunc state = 
            let innerType, nextState = run m state
            run (f innerType) nextState
        M innerFunc

    let (>>=) x f = bind f x

    let map f m = m >>= (fun x -> rets (f x))

    let apply mf mx =
        mf >>= (fun f -> 
        mx >>= (fun x -> 
        rets (f x)))

    let modify f = 
        let innerFunc state = 
            let newState = f state
            (), newState
        M innerFunc

    let compose g f = (fun x -> (g x) >>= f)

    let traverse f list =  // Not tail-recursive for some reason.
        let cons hd tl = hd :: tl

        let initState = rets []
        let folder head tail =
            f head >>= (fun h ->
            tail >>= (fun t ->
            rets (cons h t) ))

        List.foldBack folder list initState

    let sequence list = traverse id list

    let inline operation op m1 m2 = 
        m1 >>= (fun x1 ->
        m2 >>= (fun x2 ->
        rets (op x1 x2)))

    let inline add m1 m2 = operation ( + ) m1 m2
    let inline mult m1 m2 = operation ( * ) m1 m2

module BiMonad = 
    type M<'State1,'State2,'T,'U> = M of ('State1 -> 'State2 -> 'T * 'U * 'State1 * 'State2)

    let run (M f) initState1 initState2 = f initState1 initState2

    let rets x y = 
        let innerFunc state1 state2 = x, y, state1, state2
        M innerFunc

    let bind f m = 
        let innerFunc state1 state2 = 
            let innerType1, innerType2, nextState1, nextState2 = run m state1 state2
            run (f innerType1 innerType2) nextState1 nextState2
        M innerFunc

    let (>>=) x f = bind f x

    let map f1 f2 m = m >>= (fun x1 x2 -> rets (f1 x1) (f2 x2))

    let apply mf mx = 
        mf >>= (fun f1 f2 ->
        mx >>= (fun x1 x2 ->
        rets (f1 x1) (f2 x2)))

    let modify f = 
        let innerFunc state1 state2 = 
            let newState1, newState2 = f state1 state2
            (), (), newState1, newState2
        M innerFunc

    let modifyWithMonads m1 m2 = 
        modify (fun s1 s2 -> Monad.run m1 s1 |> snd, Monad.run m2 s2 |> snd)

    let modifyFirstWithMonad m1 = modifyWithMonads m1 (Monad.rets ())
    let modifySecondWithMonad m2 = modifyWithMonads (Monad.rets ()) m2

    let crossModify f1 f2 = 
        let innerFunc state1 state2 = 
            (),(),(f1 state1 state2),(f2 state1 state2)
        M innerFunc

    let compose g f = (fun x1 x2 -> (g x1 x2) >>= f)

    let inline operation op m1 m2 = 
        m1 >>= (fun x1 x2 ->
        m2 >>= (fun y1 y2 ->
        rets (op x1 x2) (op y1 y2)))

    let inline add m1 m2 = operation ( + ) m1 m2
    let inline mult m1 m2 = operation ( * ) m1 m2
            

