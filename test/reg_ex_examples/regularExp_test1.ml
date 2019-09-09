let module M = RegularExp.Make in 
let l1 = [1;2; 3] in 
let l2 = [1;51;1] in 
let output1 = M.regConcat l1 l2 in 
let output2 = M.regAlteration l1 l2 in 
print_newline();
print_newline();
M.print_pairList output1;
(*print_newline();
print_newline();
M.print_rExp print_int output2;
print_newline();
print_newline();
M.print_rExp print_int (M.anchor_start 1 l1);
print_newline();
print_newline();
M.print_rExp print_int (M.anchor_end 1 l2);
print_newline ();
print_newline();
M.print_rExp print_int (M.anchor_start_end 1 1 l2);
print_newline ();
print_newline();
M.print_rExp print_int (M.anchor_exact [1;1223;13] l1);
print_newline ();
print_newline ();
print_newline();*)
print_int (M.last_element l1);
M.print_rExp print_int (M.reg_zero_one 1 2 l1);
(*print_newline ();
print_newline();
M.print_rExp (M.reg_star 1 2 l1);
print_newline ();
print_newline();
M.print_rExp (M.reg_add 1 2 l1);
print_newline ();
print_newline();
M.print_rExp (M.reg_zero_one 1 2 l1);
print_newline ();
print_newline();
Printf.printf "%B" (M.get_next_next_element 1 123 l1) *)


