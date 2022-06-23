
module Serialization

open ByteUtils
open Digest
open Ecc
open EllipticCurve
open System
open Types

type Compression = Compressed | Uncompressed

let toSec compression (PubKey (x, y)) =
    match compression with
    | Uncompressed -> List.concat [[0x04uy]; bigintToBytes x; bigintToBytes y]
    | Compressed ->
        let prefix =
            match y.IsEven with
            | true -> 0x02uy
            | false -> 0x03uy
        List.concat [[prefix]; bigintToBytes x]

let secToPubKey (x: byte list) =
    match x with
    | 0x04uy :: tail ->
        let x = tail |> List.take numberByteSize |> bytesToBigint
        let y = tail |> List.skip numberByteSize |> bytesToBigint
        PubKey (x, y)
    | 0x02uy :: tail ->
        let x = bytesToBigint tail
        let y = calculateYCoordinate secp256k1 x |> fst
        PubKey (x, y)
    | 0x03uy :: tail ->
        let x = bytesToBigint tail
        let y = calculateYCoordinate secp256k1 x |> snd
        PubKey (x, y)
    | _ -> invalidArg "x" "Prefix has to be `02`, `03` or `04`"

let toDer (Signature (r, s)) = 
    let encodeNumber n =
        let marker = 0x02uy
        let bytes = bigintToBytes n
        let bytesWithLeadingZero =
            if List.head bytes >= 0x80uy then 0x00uy :: bytes else bytes
        let valueLength = List.length bytesWithLeadingZero |> byte
        marker :: valueLength :: bytesWithLeadingZero

    let marker = 0x30uy
    let values = List.concat [encodeNumber r; encodeNumber s]
    let sigLength = List.length values |> byte
    marker :: sigLength :: values

open Reader
let readDerSignature =
    let readNumber =
        reader {
            let! marker = readByte
            if marker <> 0x02uy then return Error "Unexpected marker"
            let! lenght = readByte
            let! bytes = readBytes (int lenght)
            return Ok (bytesToBigint bytes)
        }
    reader {
        let! marker = readByte
        if marker <> 0x30uy then return Error "Unexpected marker"
        let! _ = readByte // sigLength
        let! r = readNumber
        let! s = readNumber
        return Ok (Signature (r, s))
    }

let derToSignature bytes = run readDerSignature bytes

let encodeBase58 bytes =
    let base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

    let numberOfLeadingZeros = bytes |> List.takeWhile ((=) 0x00uy) |> List.length
    let prefix = String.replicate numberOfLeadingZeros "1"

    let num = bytesToBigint bytes
    let rec loop n result =
        if n > 0I then
            let (quotient, remainder) = bigint.DivRem(n, 58I)
            loop quotient (base58Alphabet.[int remainder] :: result)
        else
            result
    let tail = loop num [] |> List.toArray |> String

    prefix + tail

let encodeBase58WithCheckSum bytes =
    let checkSum = bytes |> hash256 |> List.take 4
    [bytes; checkSum] |> List.concat |> encodeBase58

let toAddress net compression =
    let netPrefix = 
        match net with
        | MainNet -> 0x00uy
        | TestNet -> 0x6fuy
    toSec compression >> hash160 >> List.append [netPrefix] >> encodeBase58WithCheckSum

let toWifFormat net compressionForPubKey (PrivKey secret) =
    let netPrefix =
        match net with
        | MainNet -> 0x80uy
        | TestNet -> 0xefuy
    let value = secret |> bigintToBytes |> makeWithLeading numberByteSize 0x00uy
    let compressionSuffix = if compressionForPubKey = Compressed then [0x01uy] else []
    [[netPrefix]; value; compressionSuffix] |> List.concat |> encodeBase58WithCheckSum
