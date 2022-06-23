module Reader

open System

type SeqPosition = {
    seq: seq<byte>
    position: int
}

type ReaderResult<'T> = Result<'T, string>

type Reader<'T> =
    Reader of (SeqPosition -> SeqPosition * ReaderResult<'T>)

module Reader =
    let Return value: Reader<'T> =
        Reader (fun position -> position, value)

    let Bind (Reader reader) (func: 'T -> Reader<'U>): Reader<'U> =
        let foo position =
            let newPosition, result = reader position
            match result with
            | Ok value ->
                let (Reader newReader) = func value
                newReader newPosition
            | Error message -> newPosition, Error message
        Reader foo

    let Yield (func: unit -> Reader<'T>): Reader<'T> =
        let foo position =
            let (Reader reader) = func()
            reader position
        Reader foo

type ReaderBuilder() =
    member x.Return value = Reader.Return value
    member x.Bind(reader, func) = Reader.Bind reader func
    // member x.Yield(func) = Reader.Yield func
    member x.ReturnFrom a = a
    member x.Zero() = x.Return (Ok ())
    member x.Combine(r1, r2) = x.Bind(r1, fun () -> r2)
    member x.Delay(f) = f (Ok ())

let reader = ReaderBuilder()

let run (Reader reader) (bytes: seq<byte>): ReaderResult<'T> =
    let seqPosition = {seq = bytes; position = 0}
    let _, reader = reader seqPosition
    reader

let runWithContinuation (Reader reader) (bytes: seq<byte>) =
    let seqPosition = {seq = bytes; position = 0}
    reader seqPosition

let readBytes count =
    Reader (fun seqPosition ->
        let init = seqPosition.seq |> Seq.truncate count |> Seq.toList
        if Seq.length init = count then
            let tail = Seq.skip count seqPosition.seq
            {seq = tail; position = seqPosition.position + count}, Ok init
        else
            let message = String.Format("seq has not enough elements to read {0} elements", count)
            seqPosition, Error message
    )

let readByte =
    reader {
        let! bytes = readBytes 1
        return Ok (List.head bytes)
    }

let loop count func =
    let rec loopImpl index acc =
        reader {
            if index = 0 then
                return acc |> List.rev |> Ok
            else
                let! x = func
                return! loopImpl (index - 1) (x :: acc)
        }
    loopImpl count []

let private position = Reader (fun position -> position, Ok position.position)

let private readUntil endPosition func =
    let rec loop acc =
        reader {
            let! curPosition = position
            if curPosition = endPosition then
                return acc |> List.rev |> Ok
            elif curPosition > endPosition then
                return Error "readUntil read more then was required ()"
            else
                let! x = func
                return! loop (x :: acc)
        }
    loop []

let readWithLimit limit func =
    reader {
        let! start = position
        return! readUntil (start + limit) func
    }
