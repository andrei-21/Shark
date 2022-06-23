module SerializationTest

open ByteUtils
open Ecc
open Serialization
open Types
open Xunit

let hexToBytes = hexToBigint >> bigintToBytes

[<Fact>]
let ``sec serialization`` () =
    let privKey = PrivKey 12345I
    let pubKey = computePubKey privKey
    Assert.Equal(
        "04f01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f0eba29d0f0c5408ed681984dc525982abefccd9f7ff01dd26da4999cf3f6a295",
        toSec Uncompressed pubKey |> bytesToHex
    )
    Assert.Equal(
        "03f01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f",
        toSec Compressed pubKey |> bytesToHex
    )

[<Fact>]
let ``sec deserialization`` () =
    let x = hexToBigint "f01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f"
    let y = hexToBigint "0eba29d0f0c5408ed681984dc525982abefccd9f7ff01dd26da4999cf3f6a295"
    let pubKey = PubKey (x, y)
    Assert.Equal(
        pubKey,
        secToPubKey (hexToBytes "04f01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f0eba29d0f0c5408ed681984dc525982abefccd9f7ff01dd26da4999cf3f6a295")
    )
    Assert.Equal(
        pubKey,
        secToPubKey (hexToBytes "03f01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f")
    )

[<Fact>]
let ``der serialization`` () =
    let r = hexToBigint "ed8188192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f"
    let s = hexToBigint "7a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed"
    Assert.Equal(
        "3045022100ed8188192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed",
        toDer (Signature (r, s)) |> bytesToHex
    )

[<Fact>]
let ``encode base58`` () =
    Assert.Equal("1", encodeBase58 [0x00uy])
    Assert.Equal("A", encodeBase58 [0x09uy])
    Assert.Equal("3v", encodeBase58 [0xa9uy])
    Assert.Equal("111", encodeBase58 [0x00uy; 0x00uy; 0x00uy])

    Assert.Equal(
        "9MA8fRQrT4u8Zj8ZRd6MAiiyaxb2Y1CMpvVkHQu5hVM6",
        encodeBase58 (hexToBytes "7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d")
    )
    Assert.Equal(
        "4fE3H2E6XMp4SsxtwinF7w9a34ooUrwWe4WsW1458Pd",
        encodeBase58 (hexToBytes "eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c")
    )
    Assert.Equal(
        "EQJsjkd6JaGwxrjEhfeqPenqHwrBmPQZjJGNSCHBkcF7",
        encodeBase58 (hexToBytes "c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6")
    )

[<Fact>]
let ``to address`` () =
    let privKeyToAddres net compression = PrivKey >> computePubKey >> toAddress net compression

    Assert.Equal("mmTPbXQFxboEtNRkwfh6K51jvdtHLxGeMA", privKeyToAddres TestNet Uncompressed 5002I)
    Assert.Equal("mopVkxp8UhXqRYbCYJsbeE1h1fiF64jcoH",
        privKeyToAddres TestNet Compressed (bigint.Pow(2020I, 5))
    )
    Assert.Equal("1F1Pn2y6pDb68E5nYJJeba4TLg2U7B6KF1",
        privKeyToAddres MainNet Compressed (hexToBigint "12345deadbeef")
    )

[<Fact>]
let ``to WIF format`` () =
    Assert.Equal(
        "cMahea7zqjxrtgAbB7LSGbcQUr1uX1ojuat9jZodMN8rFTv2sfUK",
        toWifFormat TestNet Compressed (PrivKey 5003I))
    Assert.Equal(
        "91avARGdfge8E4tZfYLoxeJ5sGBdNJQH4kvjpWAxgzczjbCwxic",
        toWifFormat TestNet Uncompressed (PrivKey (bigint.Pow(2021I, 5))))
    Assert.Equal(
        "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgiuQJv1h8Ytr2S53a",
        toWifFormat MainNet Compressed (PrivKey (hexToBigint "54321deadbeef")))
