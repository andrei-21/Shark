module EllipticCurve

open ByteUtils
open System

type FiniteField = Order of bigint
let order (Order o) = o

type EllipticCurve =
    private {
        a: bigint
        b: bigint
        field: FiniteField
    }
    override this.ToString() =
        String.Format("EllipticCurve ({0},{1}) over {2}",
            this.a, this.b, this.field)
    static member Create a b field =
        if a >= order field || b >= order field then
            invalidArg "a b" "Coeffs have to be less then the field order"
        else
            {a = a; b = b; field = field}

type Point =
    | Point of x: bigint * y: bigint
    | Infinity

type PointOnEllipticCurve =
    {
        curve: EllipticCurve
        point: Point
    }
    override this.ToString() =
        String.Format("{0} @ {1}", this.point, this.curve)
    static member Create curve point =
        match point with
        | Infinity -> {curve = curve; point = point}
        | Point (x, y) ->
            let order = order curve.field
            let l = (pown y 2)
            let r = (pown x 3) + curve.a * x + curve.b
            if umodulo l order = umodulo r order then
                {curve = curve; point = point}
            else
                invalidArg "point" "Point is not on the curve"

let calculateYCoordinate curve (x: bigint) =
    let order = order curve.field
    let sqrt n = bigint.ModPow(n, ((order + 1I) / 4I), order)
    let alpha = umodulo (pown x 3 + curve.b) order
    let beta = sqrt alpha
    if beta.IsEven then
        (beta, order - beta)
    else
        (order - beta, beta)

let add l r =
    let add x1 y1 x2 y2 =
        let order = order l.curve.field
        let div = moduloDiv order
        if x1 = x2 && y1 <> y2 then
            Infinity
        else if x1 = x2 && y1 = 0I && y2 = 0I then
            Infinity
        else if x1 = x2 && y1 = y2 then
            let s = div (3I * x1 * x1 + l.curve.a) (2I * y1)
            let x3 = umodulo (s * s - 2I * x1) order
            let y3 = umodulo (s * (x1 - x3) - y1) order
            Point (x3, y3)
        else
            let s = div (y2 - y1) (x2 - x1)
            let x3 = umodulo (s * s - x1 - x2) order
            let y3 = umodulo (s * (x1 - x3) - y1) order
            Point (x3, y3)
    if l.curve = r.curve then
        match l.point, r.point with
        | Infinity, _ -> r
        | _, Infinity -> l
        | Point (x1, y1), Point (x2, y2) -> {l with point = add x1 y1 x2 y2}
    else
        invalidArg "l r" "Points have to be on the same curve"

let rmul coef p =
    let rec loop i p result =
        if i = numberByteSize * 8 then
            result
        else
            let dp = add p p
            if (coef >>> i) &&& 1I = 1I then
                loop (i + 1) dp (add p result)
            else
                loop (i + 1) dp result
    loop 0 p {p with point = Infinity}