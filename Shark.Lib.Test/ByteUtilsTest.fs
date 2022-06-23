module ByteUtilsTest

open ByteUtils
open Xunit

[<Fact>]
let ``umodulo`` () =
    Assert.Equal(umodulo 0I 2I, 0I)
    Assert.Equal(umodulo -1I 2I, 1I)
    Assert.Equal(umodulo -2I 2I, 0I)
    Assert.Equal(umodulo -3I 2I, 1I)
    Assert.Equal(umodulo -109871I 2I, 1I)
    Assert.Equal(umodulo -1823784375023I 3542432435I, 568329002I)
    Assert.Equal(umodulo -18237I 3542432435I, 3542414198I)

[<Fact>]
let ``moduloDiv`` () =
    Assert.Equal(moduloDiv 3I 1I 2I, 2I)
    Assert.Equal(moduloDiv 3I 2I 2I, 1I)
    
    // 0 3 1 4 2 0 4
    Assert.Equal(moduloDiv 5I 0I 3I, 0I)
    Assert.Equal(moduloDiv 5I 1I 3I, 2I)
    Assert.Equal(moduloDiv 5I 2I 3I, 4I)
    Assert.Equal(moduloDiv 5I 3I 3I, 1I)
    Assert.Equal(moduloDiv 5I 4I 3I, 3I)

    // 0 2 4 1 3
    Assert.Equal(moduloDiv 5I 0I 2I, 0I)
    Assert.Equal(moduloDiv 5I 1I 2I, 3I)
    Assert.Equal(moduloDiv 5I 2I 2I, 1I)
    Assert.Equal(moduloDiv 5I 3I 2I, 4I)
    Assert.Equal(moduloDiv 5I 4I 2I, 2I)

[<Fact>]
let ``bigintToHex`` () =
    Assert.Equal(bigintToHex 0I, "0000000000000000000000000000000000000000000000000000000000000000")
    Assert.Equal(bigintToHex 1I, "0000000000000000000000000000000000000000000000000000000000000001")
    Assert.Equal(bigintToHex 2I, "0000000000000000000000000000000000000000000000000000000000000002")
    Assert.Equal(bigintToHex 15I, "000000000000000000000000000000000000000000000000000000000000000f")
    Assert.Equal(bigintToHex 16I, "0000000000000000000000000000000000000000000000000000000000000010")
    Assert.Equal(bigintToHex 1024I, "0000000000000000000000000000000000000000000000000000000000000400")
    Assert.Equal(bigintToHex 1026I, "0000000000000000000000000000000000000000000000000000000000000402")
    Assert.Equal(bigintToHex 09298013443534523I, "0000000000000000000000000000000000000000000000000021087e746b2ebb")

    let gx = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
    let gy = "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
    let n = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"
    Assert.Equal(gx |> hexToBigint |> bigintToHex, gx)
    Assert.Equal(gy |> hexToBigint |> bigintToHex, gy)
    Assert.Equal(n |> hexToBigint |> bigintToHex, n)

[<Fact>]
let ``intermediateSums`` () =
    Assert.Equal<int>([], intermediateSums id 1 0)
    Assert.Equal<int>([1], intermediateSums id 1 1)
    Assert.Equal<int>([1; 1], intermediateSums id 1 2)
    Assert.Equal<int>([1; 1; 1;], intermediateSums id 1 3)

    Assert.Equal<int>([4; 3; 2; 1], intermediateSums ((+) 1) 1 4)
    Assert.Equal<int>([8; 4; 2; 1], intermediateSums (fun x -> x + x) 1 4)
    Assert.Equal<int>([16 * 16; 16; 4; 2], intermediateSums (fun x -> x * x) 2 4)

[<Fact>]
let ``bigintToBits`` () =
    Assert.Equal<bool>([], bigintToBits 0I)
    Assert.Equal<bool>([true], bigintToBits 1I)
    Assert.Equal<bool>([true; false], bigintToBits 2I)
    Assert.Equal<bool>(
        [true; false; false; false; false; false; false; false; false; false; false],
        bigintToBits 1024I)
