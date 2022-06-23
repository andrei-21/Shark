module DigestTest

open ByteUtils
open Digest
open System
open Xunit

let private applyHash hash (s: String) =
    s |> Text.Encoding.ASCII.GetBytes |> hash |> Array.toList |> bytesToHex

[<Fact>]
let ``ripemd160`` () =
    let hasher = applyHash ripemd160
    Assert.Equal(
        "37f332f68db77bd9d7edd4969571ad671cf9dd3b",
        hasher "The quick brown fox jumps over the lazy dog")
    Assert.Equal(
        "132072df690933835eb8b6ad0b77e7b6f14acad7",
        hasher "The quick brown fox jumps over the lazy cog")
    Assert.Equal(
        "9c1185a5c5e9fc54612808977ee8f548b2258d31",
        hasher "")
