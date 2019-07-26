[
  structure_item (concat.ml[1,0+0]..concat.ml[4,51+30])
    Tstr_value Rec
    [
      <def>
        pattern (concat.ml[1,0+8]..concat.ml[1,0+14])
          Tpat_var "concat/1199"
        expression (concat.ml[1,0+15]..concat.ml[4,51+30]) ghost
          Texp_function
          Nolabel
          [
            <case>
              pattern (concat.ml[1,0+15]..concat.ml[1,0+17])
                Tpat_var "l1/1200"
              expression (concat.ml[1,0+18]..concat.ml[4,51+30]) ghost
                Texp_function
                Nolabel
                [
                  <case>
                    pattern (concat.ml[1,0+18]..concat.ml[1,0+20])
                      Tpat_var "l2/1201"
                    expression (concat.ml[2,24+0]..concat.ml[4,51+30])
                      Texp_match
                      expression (concat.ml[2,24+6]..concat.ml[2,24+8])
                        Texp_ident "l1/1200"
                      [
                        <case>
                          pattern (concat.ml[3,38+4]..concat.ml[3,38+6])
                            Tpat_construct "[]"
                            []
                          expression (concat.ml[3,38+10]..concat.ml[3,38+12])
                            Texp_ident "l2/1201"
                        <case>
                          pattern (concat.ml[4,51+4]..concat.ml[4,51+9])
                            Tpat_construct "::"
                            [
                              pattern (concat.ml[4,51+4]..concat.ml[4,51+5])
                                Tpat_var "x/1202"
                              pattern (concat.ml[4,51+7]..concat.ml[4,51+9])
                                Tpat_var "xs/1203"
                            ]
                          expression (concat.ml[4,51+13]..concat.ml[4,51+30])
                            Texp_construct "::"
                            [
                              expression (concat.ml[4,51+13]..concat.ml[4,51+14])
                                Texp_ident "x/1202"
                              expression (concat.ml[4,51+16]..concat.ml[4,51+30])
                                Texp_apply
                                expression (concat.ml[4,51+17]..concat.ml[4,51+23])
                                  Texp_ident "concat/1199"
                                [
                                  <arg>
                                    Nolabel
                                    expression (concat.ml[4,51+24]..concat.ml[4,51+26])
                                      Texp_ident "xs/1203"
                                  <arg>
                                    Nolabel
                                    expression (concat.ml[4,51+27]..concat.ml[4,51+29])
                                      Texp_ident "l2/1201"
                                ]
                            ]
                      ]
                      []
                ]
          ]
    ]
]

