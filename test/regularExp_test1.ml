let module M = RegularExp.Make in 
let l1 = [1;1223;13] in 
let l2 = [21;51;162] in 
let output1 = M.regConcat l1 l2 in 
let output2 = M.regAlteration l1 l2 in 
M.print_rExp output1;
print_newline();
print_newline();
M.print_rExp output2;
print_newline();
print_newline();
M.print_rExp l1;
print_newline();
print_newline();
M.print_rExp (M.anchor_start 1 l1);
print_newline();
print_newline();
M.print_rExp (M.anchor_end 1 l2);
print_newline ();
print_newline();
M.print_rExp (M.anchor_start_end 1 2 l2);
print_newline ();
print_newline();
M.print_rExp (M.anchor_exact 1 l1);
print_newline ();
print_newline ();
print_newline();
M.print_rExp (M.reg_or 1 2 3 l1);
print_newline ();
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
Printf.printf "%B" (M.get_next_next_element 1 123 l1)


