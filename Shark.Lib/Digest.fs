module Digest

open System

let ripemd160 bytes =
    let digest = Org.BouncyCastle.Crypto.Digests.RipeMD160Digest()
    Array.map digest.Update bytes |> ignore
    let size = digest.GetDigestSize()
    let out = Array.create size 0x00uy
    digest.DoFinal(out, 0) |> ignore
    out

let hash160 =
    let sha256 = Security.Cryptography.SHA256Managed.Create()
    List.toArray >> sha256.ComputeHash >> ripemd160 >> Array.toList

let hash256 =
    let hasher = Security.Cryptography.SHA256Managed.Create()
    List.toArray >> hasher.ComputeHash >> hasher.ComputeHash >> Array.toList
