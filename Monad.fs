namespace Monads

module Monad =

    type M<'State,'T> = M of ('State -> 'T * 'State)

    let run (M f) initialState = f initialState
    let get = M(fun s -> s,s)
    let put newState = M(fun _ -> (), newState)

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

    let chain g f = g >>= (fun _ -> f)

    let traverse f list =  // Not tail-recursive for some reason.
        let cons hd tl = hd :: tl

        let initState = rets []
        let folder head tail =
            f head >>= (fun h ->
            tail >>= (fun t ->
            rets (cons h t) ))

        List.foldBack folder list initState

    let sequence list = traverse id list

    let mapM arrayM = 
        let innerFunc state = Array.mapFold (fun s x -> run x s) state arrayM 
        M innerFunc

    let inline operation op m1 m2 = 
        m1 >>= (fun x1 ->
        m2 >>= (fun x2 ->
        rets (op x1 x2)))

    let inline add m1 m2 = operation ( + ) m1 m2
    let inline mult m1 m2 = operation ( * ) m1 m2
    let inline sub m1 m2 = operation ( - ) m1 m2
    let inline div m1 m2 = operation ( / ) m1 m2

    type StateBuilder() = 
        member this.Zero () = M(fun s -> (),s)
        member this.Return x = M(fun s -> x,s)
        member inline this.ReturnFrom (x:M<'s,'a>) = x
        member this.Bind (x,f):M<'s,'b> = bind f x
        member this.Combine (x1:M<'s,'a>, x2:M<'s,'b>) = 
            M(fun state -> let _,state = run x1 state
                           run x2 state)
        member this.Delay f:M<'s,'a> = f ()
        member this.For (seq,(f:'a -> M<'s,'b>)) = 
            seq |> Seq.map f
                |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1,x2))
        member this.While (f,x) = 
            if f () then this.Combine (x,this.While(f,x))
            else this.Zero()

    let state = new StateBuilder()


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

    let compose g f = (fun x1 x2 -> (g x1 x2) >>= f)

    let inline operationPair op1 op2 m1 m2 = 
        m1 >>= (fun x1 x2 ->
        m2 >>= (fun y1 y2 ->
        rets (op1 x1 y1) (op2 x2 y2)))

    let inline operationSingle op = operationPair op op 

    let inline add m1 m2 = operationSingle ( + ) m1 m2
    let inline mult m1 m2 = operationSingle ( * ) m1 m2
    let inline sub m1 m2 = operationSingle ( - ) m1 m2

    // The newly created monad has the same second element as the second monad input
    let inline add1 m1 m2 = operationPair ( + ) (fun _ y2 -> y2) m1 m2
    let inline mult1 m1 m2 = operationPair ( * ) (fun _ y2 -> y2) m1 m2

    // The newly created monad has the same first element as the second monad input
    let inline add2 m1 m2 = operationPair (fun _ y1 -> y1) ( + ) m1 m2
    let inline mult2 m1 m2 = operationPair (fun _ y1 -> y1) ( * ) m1 m2
            

