module Interpreter

open ResultWithState

let stack = resultWithState

let push value =
    M (fun stack ->
        value :: stack, Ok ()
    )

let pop =
    M (fun stack ->
        match stack with
        | head :: tail -> tail, Ok head
        | _ -> stack, Error "Empty stack"
    )

let eval (M m) (state) =
    m state
