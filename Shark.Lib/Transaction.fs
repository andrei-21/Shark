module Tx

open Reader
open Types
open Script

type TxIn = {
    prevTx: byte list
    prevIndex: int
    scriptSig: Op list
    sequence: uint64
}

type TxOut = {
    amount: uint64
    scriptPubKey: Op list
}

type Tx = {
    version: uint64
    ins: TxIn list
    outs: TxOut list
    locktime: uint64
    net: Net
}

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

let readVarInt =
    reader {
        let! firstByte = readByte
        let intSize =
            match firstByte with
            | 0xffuy -> 8
            | 0xfeuy -> 4
            | 0xfduy -> 2
            | _ -> 0
        if intSize = 0 then
            return Ok (uint64 firstByte)
        else
            return! readInt intSize LittleEndian
    }

let readTxIn =
    reader {
        let! txIdBytes = readBytes 32
        let txId = txIdBytes |> List.rev
        let! index = readInt 4 LittleEndian

        let! size = readVarInt
        let! scriptSig = readWithLimit (int size) readOp

        let! sequence = readInt 4 LittleEndian

        let txIn = {prevTx = txId; prevIndex = int index; scriptSig = scriptSig; sequence = sequence}
        return Ok txIn
    }

let readTxOut =
    reader {
        let! amount = readInt 8 LittleEndian

        let! size = readVarInt
        let! scriptPubKey = readWithLimit (int size) readOp

        let txOut = {amount = amount; scriptPubKey = scriptPubKey}
        return Ok txOut
    }

let readTx =
    reader {
        let! version = readInt 4 LittleEndian

        let! insNumber = readVarInt
        if insNumber = uint64 0 then
            return Error "number of tx inputs must be greater that zero"
        let! ins = loop (int insNumber) readTxIn

        let! outsNumber = readVarInt
        if outsNumber = uint64 0 then
            return Error "number of tx outputs must be greater that zero"
        let! outs = loop (int outsNumber) readTxOut

        let! locktime = readInt 4 LittleEndian

        let tx = {version = version; ins = ins; outs = outs; locktime = locktime; net = TestNet}
        return Ok tx
    }

let readAndPrint =
    1
    // let bytes = Seq.initInfinite (fun _ -> 0x01uy)
    // // let bytes = seq { 0x00uy .. 0xffuy }
    // let reader, result = runWithContinuation readTx bytes
    // printfn "Result: %A" result
    // printfn "Bytes consumed: %A" reader.position
    // printfn "Remaining data: %A" reader.seq
