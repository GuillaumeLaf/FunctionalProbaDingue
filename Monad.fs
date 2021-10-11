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

    let mapFoldM arrayM = 
        let innerFunc state = Array.mapFold (fun s x -> run x s) state arrayM 
        M innerFunc

    let mapM arrayM = 
        let innerFunc state = Array.Parallel.map (fun x -> run x state |> fst) arrayM, state 
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

