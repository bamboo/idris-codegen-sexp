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