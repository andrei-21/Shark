module ResultWithState

type M<'S, 'T, 'E> = M of ('S -> 'S * Result<'T, 'E>)

module ResultWithState =
    let Return value: M<'S, 'T, 'E> = M (fun state -> state, value)

    let Bind (M m) func =
        M (fun state ->
            let newState, result = m state
            match result with
            | Ok value ->
                let (M nextM) = func value
                nextM newState
            | Error error -> newState, Error error
        )

type ResultWithStateBuilder() =
    member x.Return value = ResultWithState.Return value
    member x.Bind(m, func) = ResultWithState.Bind m func
    member x.ReturnFrom a = a
    member x.Zero() = x.Return (Ok ())
    member x.Combine(r1, r2) = x.Bind(r1, fun () -> r2)
    member x.Delay(f) = f (Ok ())

let resultWithState = ResultWithStateBuilder()

let eval (M m) (state) =
    let _, result = m state
    result
