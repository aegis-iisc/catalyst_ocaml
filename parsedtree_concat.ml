[
  structure_item (concat.ml[1,0+0]..[4,51+30])
    Pstr_value Rec
    [
      <def>
        pattern (concat.ml[1,0+8]..[1,0+14])
          Ppat_var "concat" (concat.ml[1,0+8]..[1,0+14])
        expression (concat.ml[1,0+15]..[4,51+30]) ghost
          Pexp_fun
          Nolabel
          None
          pattern (concat.ml[1,0+15]..[1,0+17])
            Ppat_var "l1" (concat.ml[1,0+15]..[1,0+17])
          expression (concat.ml[1,0+18]..[4,51+30]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (concat.ml[1,0+18]..[1,0+20])
              Ppat_var "l2" (concat.ml[1,0+18]..[1,0+20])
            expression (concat.ml[2,24+0]..[4,51+30])
              Pexp_match
              expression (concat.ml[2,24+6]..[2,24+8])
                Pexp_ident "l1" (concat.ml[2,24+6]..[2,24+8])
              [
                <case>
                  pattern (concat.ml[3,38+4]..[3,38+6])
                    Ppat_construct "[]" (concat.ml[3,38+4]..[3,38+6])
                    None
                  expression (concat.ml[3,38+10]..[3,38+12])
                    Pexp_ident "l2" (concat.ml[3,38+10]..[3,38+12])
                <case>
                  pattern (concat.ml[4,51+4]..[4,51+9])
                    Ppat_construct "::" (concat.ml[4,51+5]..[4,51+7])
                    Some
                      pattern (concat.ml[4,51+4]..[4,51+9]) ghost
                        Ppat_tuple
                        [
                          pattern (concat.ml[4,51+4]..[4,51+5])
                            Ppat_var "x" (concat.ml[4,51+4]..[4,51+5])
                          pattern (concat.ml[4,51+7]..[4,51+9])
                            Ppat_var "xs" (concat.ml[4,51+7]..[4,51+9])
                        ]
                  expression (concat.ml[4,51+13]..[4,51+30])
                    Pexp_construct "::" (concat.ml[4,51+14]..[4,51+16])
                    Some
                      expression (concat.ml[4,51+13]..[4,51+30]) ghost
                        Pexp_tuple
                        [
                          expression (concat.ml[4,51+13]..[4,51+14])
                            Pexp_ident "x" (concat.ml[4,51+13]..[4,51+14])
                          expression (concat.ml[4,51+16]..[4,51+30])
                            Pexp_apply
                            expression (concat.ml[4,51+17]..[4,51+23])
                              Pexp_ident "concat" (concat.ml[4,51+17]..[4,51+23])
                            [
                              <arg>
                              Nolabel
                                expression (concat.ml[4,51+24]..[4,51+26])
                                  Pexp_ident "xs" (concat.ml[4,51+24]..[4,51+26])
                              <arg>
                              Nolabel
                                expression (concat.ml[4,51+27]..[4,51+29])
                                  Pexp_ident "l2" (concat.ml[4,51+27]..[4,51+29])
                            ]
                        ]
              ]
    ]
]

