module ScriptTest

open Script
open Xunit

[<Fact>]
let ``eval script`` () =
    Assert.Equal(Some "Empty stack at the end", evalScript [])
    Assert.Equal(Some "Empty stack", evalScript [OP_DUP])
    Assert.Equal(Some "OP_VERIFY failed", evalScript [Element []; OP_VERIFY])
    Assert.Equal(None, evalScript [Element [0x01uy]; Element [0x01uy]; OP_VERIFY])
    Assert.Equal(None,
        evalScript [
            Element [0x01uy];
            Element [0x02uy];
            OP_ADD;
            Element [0x03uy];
            OP_EQUALVERIFY;
            Element [0x01uy]
        ])
