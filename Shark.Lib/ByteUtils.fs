module ByteUtils

open System

let numberByteSize = 32

let makeWithLeading count element l =
    let remainder = count - List.length l
    let leadingElements = List.replicate remainder element
    List.append leadingElements l

let umodulo n m: bigint = ((n % m) + m) % m
let moduloDiv modulo l r =
    let inverse = bigint.ModPow (r, modulo - 2I, modulo)
    umodulo (l * inverse) modulo

let hexToBigint s = bigint.Parse("0" + s, Globalization.NumberStyles.HexNumber)

let bigintToBytes (n: bigint) =
    n.ToByteArray(true, true) |> Array.toList

let bytesToBigint bytes =
    let byteArray = List.toArray bytes
    bigint(ReadOnlySpan<byte>(byteArray), true, true)

let bytesToHex =
    let byteToHex (x: byte) = String.Format("{0:x2}", x)
    List.map byteToHex >> String.concat ""

let bigintToHex = bigintToBytes >> makeWithLeading 32 0x00uy >> bytesToHex

let private randomBigint (rnd: Random) =
    let randomByte i = bigint (rnd.Next() % 0xff) <<< (i * 8)
    [0 .. numberByteSize - 1]
    |> List.mapi (fun i _ -> randomByte i)
    |> List.sum

let intermediateSums op baseElement number =
    let rec loop index element result =
        match index with
        | 0 -> result
        | _ -> loop (index - 1) (op element) (element :: result)
    loop number baseElement []

let bigintToBits (n: bigint) =
    let toBitArray (b: byte) =
        let rec loop index result =
            match index with
            | 8 -> result
            | _ -> loop (index + 1) ((b >>> index &&& 1uy = 1uy) :: result)
        loop 0 []
    n.ToByteArray() |> Array.toList |> List.rev |> List.collect toBitArray |> List.skipWhile not
