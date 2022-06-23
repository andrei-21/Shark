# Shark — Bitcoin Full Node Implementation in F#

An attempt to implement Bitcoin full node in F# for educational purposes.

The objectives are to develop simple and clean code and to use as few dependencies as possible.

## Snippets

### Implementing Hash256 Using Composition Operator

```fs
let hash256 =
    let hasher = Security.Cryptography.SHA256Managed.Create()
    List.toArray >> hasher.ComputeHash >> hasher.ComputeHash >> Array.toList
```

### Parsing Transaction Using Reader Monad

```fs
let readTx =
    reader {
        let! version = readInt 4 LittleEndian

        let! insNumber = readVarInt
        if insNumber = uint64 0 then
            return Error "number of tx inputs must be greater that zero"
        let! ins = loop (int insNumber) readTxIn

        let! outsNumber = readVarInt
        if outsNumber = uint64 0 then
            return Error "number of tx outputs must be greater that zero"
        let! outs = loop (int outsNumber) readTxOut

        let! locktime = readInt 4 LittleEndian

        let tx = {version = version; ins = ins; outs = outs; locktime = locktime; net = TestNet}
        return Ok tx
    }
```

### Evaluating Script Operator Using Stack Monad

```fs
let evalOp op =
    stack {
        match op with
        // ...
        | OP_ADD ->
            let! a = popNumber
            let! b = popNumber
            return! pushNumber (a + b)
        // ...
```

## Dependencies

 * `System.Numerics.BigInteger` from .Net Standard
 * `System.Security.Cryptography.SHA256Managed` from .Net Standard
 * `Org.BouncyCastle.Crypto.Digests.RipeMD160Digest` from https://bouncycastle.org

## Working With Solution

### Setup Solution

```sh
# Create solution
dotnet new sln -o Shark
cd Shark/

# Create projects
dotnet new console -o Shark.Client -lang 'F#'
dotnet new classlib -o Shark.Lib -lang 'F#'
dotnet new xunit -o Shark.Lib.Test -lang 'F#'

# Add projects to solution
dotnet sln add Shark.Client/Shark.Client.fsproj Shark.Lib/Shark.Lib.fsproj Shark.Lib.Test/Shark.Lib.Test.fsproj

# Add references between projects
dotnet add Shark.Lib.Test/Shark.Lib.Test.fsproj reference Shark.Lib/Shark.Lib.fsproj
dotnet add Shark.Client/Shark.Client.fsproj reference Shark.Lib/Shark.Lib.fsproj
```

### Run All Tests

```sh
dotnet test
```

### Run Client

```sh
dotnet run -p Shark.Client/Shark.Client.fsproj
# or
cd Shark.Client/ && dotnet run
```

### Restore Solution

```sh
dotnet restore
```
