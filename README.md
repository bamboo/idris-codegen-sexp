# idris-codegen-sexp

An [Idris](https://www.idris-lang.org/) backend that outputs pretty printed [Idris simplified
expressions](https://github.com/idris-lang/Idris-dev/blob/f92ecd25d48a9120abfbb7d6af81f9b194ab98f1/src/IRTS/Simplified.hs#L3), useful for backend writers.

```idris
main : IO
main = putStrLn "Hello World!"
```

Becomes:

```idris
(SFun "runMain0"
      []
      1
      (SLet (Loc 0)
            (SLet (Loc 0)
                  SNothing
                  (SApp False "Main.main" [(Loc 0)]))
            (SApp True "EVAL0" [(Loc 0)])))


(SFun "Main.main"
      ["in0"]
      1
      (SLet (Loc 1)
            (SLet (Loc 1)
                  (SConst (Str ['H','e','l','l','o',',',' ','w',
                                'o','r','l','d','!','
                                ']))
                  (SOp LWriteStr [(Loc 0),(Loc 1)]))
            (SCon Nothing 0 "MkUnit" [])))


(SFun "EVAL0"
      ["arg0"]
      1
      (SChkCase (Loc 0)
                [(SDefaultCase (SV (Loc 0)))]))
```

## Installation

    stack install

## Usage

    idris --codegen sexp test/cases/HelloWorld.idr -o HelloWorld.sexp

