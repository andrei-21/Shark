module Script

open ByteUtils
open System
open Types
open Reader
open Serialization

[<Diagnostics.CodeAnalysis.SuppressMessage("*", "UnionCasesNames")>]
type Op =
| OP_0
| OP_1
| OP_16
| OP_ADD
| OP_CHECKSIG
| OP_DUP
| OP_EQUALVERIFY
| OP_HASH160
| OP_VERIFY
| Element of bytes: byte list

let bytesToNumber endian =
    let makeEndian endian =
        match endian with
        | BigEndian -> List.rev
        | LittleEndian -> id
    let shift i (b: byte) =
        let n = uint64 b
        n <<< (i * 8)
    makeEndian endian >> Seq.mapi shift >> Seq.reduce (|||)

let readInt size endian =
    reader {
        let! bytes = readBytes size
        let number = bytes |> Seq.toList |> bytesToNumber endian
        return Ok number
    }

let readVarElement lengthSize =
    reader {
        let! length = readInt lengthSize LittleEndian
        if length > 540UL then
            return Error "Element is to big (> 540 bytes)"
        let! data = readBytes (int length)
        return Ok (Element data)
    }

let readOp =
    reader {
        match! readByte with
        | 0x00uy -> return Ok OP_0
        | n when n <= 0x4buy ->
            let! data = readBytes (int n)
            return Ok (Element data)
        | 0x4cuy -> return! readVarElement 1
        | 0x4duy -> return! readVarElement 2
        | 0x4euy -> return! readVarElement 4
        | 0x51uy -> return Ok OP_1
        | 0x60uy -> return Ok OP_16
        | 0x76uy -> return Ok OP_DUP
        | 0x88uy -> return Ok OP_EQUALVERIFY
        | 0x93uy -> return Ok OP_ADD
        | 0xa9uy -> return Ok OP_HASH160
        | 0xacuy -> return Ok OP_CHECKSIG
        | b -> return Error (String.Format("Unknown op code: 0x{0:x2}", b))
    }

open Interpreter

// TODO: Make it signed.
let popNumber =
    stack {
        let! x = pop
        let number = x |> List.rev |> bytesToBigint
        return Ok number
    }

let pushNumber number =
    stack {
        let bytes = number |> bigintToBytes |> List.rev
        return! push bytes
    }

// TODO: Read number properly.
let readVarInt =
    stack {
        match! pop with
        | [] -> return Ok 0
        | 0xffuy :: _ -> return Error "9 bytes integers are not allowed"
        | 0xfeuy :: _ -> return Error "5 bytes integers are not allowed"
        | 0xfduy :: _ -> return Ok 1 // TODO: Read 2 bytes.
        | bytes when List.length bytes > 1 -> return Error "Unxpected bytes after small integer"
        | [b] when 0b10000000uy &&& b = 0b10000000uy -> return Ok -1
        | [b] -> return Ok (int b)
        | _ -> return Error "Logic error: Unreachable code"
    }

open Ecc

let evalOp op =
    stack {
        match op with
        | OP_0 -> return! push []
        | OP_1 -> return Error "Not Implemented"
        | OP_16 -> return Error "Not Implemented"
        | OP_ADD ->
            let! a = popNumber
            let! b = popNumber
            return! pushNumber (a + b)
        | OP_CHECKSIG ->
            let! secPubKey = pop
            let pubKey = secToPubKey secPubKey
            let! derSignature = pop
            match derToSignature derSignature with
            | Ok signature ->
                let z = 1I
                let x = verify pubKey z signature
                if x then
                    return! pushNumber 1I
                else
                    return! pushNumber 0I
            | Error error -> return Error error
            return Error "Not Implemented"
        | OP_DUP ->
            let! x = pop
            do! push x
            do! push x
            return Ok ()
        | OP_EQUALVERIFY ->
            let! a = pop
            let! b = pop
            if a = b then
                return Ok ()
            else
                return Error "OP_EQUALVERIFY failed"
        | OP_HASH160 ->
            let! x = pop
            return! push (Digest.hash256 x)
        | OP_VERIFY ->
            let! x = popNumber
            if x = 0I then
                return Error "OP_VERIFY failed"
            else
                return Ok ()
        | Element bytes -> return! push bytes
    }

let evalScript ops =
    let rec loop ops =
        stack {
            match ops with
            | head :: tail ->
                do! evalOp head
                return! loop tail
            | _ -> return Ok ()
        }

    let stack, result = eval (loop ops) []
    match stack, result with
    | _ :: _, Ok _ -> None // TODO: Check the value at the top
    | [], Ok _ -> Some "Empty stack at the end"
    | _, Error error -> Some error
