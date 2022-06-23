module Ecc

open ByteUtils
open EllipticCurve

let a = 0I
let b = 7I
let p = (pown 2I 256) - (pown 2I 32) - 977I
let secp256k1 = EllipticCurve.Create a b (Order p)

let private gx = hexToBigint "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
let private gy = hexToBigint "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
let n = hexToBigint "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"

let private generatorPoint = Point (gx, gy)
let g = PointOnEllipticCurve.Create secp256k1 generatorPoint

let infinity = PointOnEllipticCurve.Create secp256k1 Infinity

type PrivKey = PrivKey of bigint

type PubKey = PubKey of x: bigint * y: bigint

let computePubKey (PrivKey privKey) =
    let pointOnCurve = rmul privKey g
    match pointOnCurve.point with
    | Point (x, y) -> PubKey (x, y)
    | Infinity -> failwith "Point is infinity for the private key (private key equals to N?)"

type Signature = Signature of r: bigint * s: bigint

let verify (PubKey (x, y)) z (Signature (r, s)) =
    let p = PointOnEllipticCurve.Create secp256k1 (Point (x, y))
    let div = moduloDiv n
    let u = div z s
    let v = div r s
    let total = add (rmul u g) (rmul v p)
    match total.point with
    | Point (x, _)  -> x = r
    | Infinity -> false

let hashKGenerator (z: bigint) =
    // TODO: Implement base on hash.
    z % n

let signer generateK (PrivKey secret) z =
    let k = generateK z
    let r =
        match (rmul k g).point with
        | Point (x, _) -> x
        | Infinity -> failwith "Logic error: got Infinity when multiplying (maybe k > n?)"
    let div = moduloDiv n
    let s = div (z + r * secret) k
    if 2I * s > n then
        Signature (r, n - s)
    else
        Signature (r, s)

let sign = signer hashKGenerator
