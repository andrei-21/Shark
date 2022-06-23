module EllipticCurveTest

open EllipticCurve
open Xunit

let secp256k1 = EllipticCurve.Create 0I 7I (Order 103I)
let infinity = PointOnEllipticCurve.Create secp256k1 Infinity

[<Fact>]
let ``point is on curve`` () =
    let p = Point (17I, 64I)
    let pp = PointOnEllipticCurve.Create secp256k1 p
    Assert.Equal(p, pp.point)
    Assert.Equal(secp256k1, pp.curve)

[<Fact>]
let ``point is NOT on curve`` () =
    let p = Point (18I, 65I)
    Assert.Throws<System.ArgumentException>(fun () -> PointOnEllipticCurve.Create secp256k1 p |> ignore)

[<Fact>]
let ``point addition`` () =
    let secp256k1 = EllipticCurve.Create 0I 7I (Order 223I)
    let pp1 = PointOnEllipticCurve.Create secp256k1 (Point (192I, 105I))
    let pp2 = PointOnEllipticCurve.Create secp256k1 (Point (17I, 56I))
    Assert.Equal(PointOnEllipticCurve.Create secp256k1 (Point (170I, 142I)), add pp1 pp2)
    Assert.Equal(PointOnEllipticCurve.Create secp256k1 (Point (170I, 142I)), add pp2 pp1)

[<Fact>]
let ``scalar multiplication`` () =
    let p = Point (17I, 64I)
    let pp = PointOnEllipticCurve.Create secp256k1 p
    Assert.Equal(pp, add pp infinity)
    Assert.Equal(pp, add infinity pp)
    Assert.Equal(infinity, rmul 0I pp)
    Assert.Equal(pp, rmul 1I pp)
    Assert.Equal(add pp pp, rmul 2I pp)
    Assert.Equal(add pp (add pp pp), rmul 3I pp)
    Assert.Equal(add pp (add pp (add pp pp)), rmul 4I pp)

    // Assert.Equal(pp, rmul 103I pp)

[<Fact>]
let ``calculate Y coordinate`` () =
    let secp256k1 = EllipticCurve.Create 0I 7I (Order 223I)
    Assert.Equal(105I, calculateYCoordinate secp256k1 192I |> snd)
    Assert.Equal(56I, calculateYCoordinate secp256k1 17I |> fst)
    Assert.Equal(142I, calculateYCoordinate secp256k1 170I |> fst)

