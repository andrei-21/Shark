module EccTest

open Ecc
open EllipticCurve
open Xunit

[<Fact>]
let ``signing and verification`` () =
    let privKey = PrivKey 12345I
    let signature = sign privKey 67890I
    let (Signature (r, s)) = signature
    let pubKey = computePubKey privKey
    Assert.True(verify pubKey 67890I signature)

    Assert.False(verify pubKey 67890I (Signature (r, 345345I)))
    Assert.False(verify pubKey 67890I (Signature (3245435I, s)))
 
    Assert.False(verify pubKey 67889I signature)
    Assert.False(verify pubKey 67891I signature)
    Assert.False(verify pubKey 678923I signature)
