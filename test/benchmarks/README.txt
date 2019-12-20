README accompanied with the mini_png example without CRC
Common global comments:
	1. The language of catalyst is monomporphic with base types int, bool and int list, thus any ocmal type 'a list is being compiled to int list
	2. $ 1 gives a description of the implementation of a mini_png.
	3. $ 2 gives a description of the specifications
	3. $ 3 gives a mapping  of informal properties to the line numbere in specifications which enforce them, example properties which cannot be checked 
		with current implementation (due to some unhandled scenarios), properties which will require fundamental extension to the theory of Rel Refinement Type.


$1. description mini_png.ml, describes monomorphic ML types for the functions
--------------------------------------------------------------------------

(*a product type for a parser return value,  (parser_output, ramaining_list)*)
type listPair  = Pair of (int list) * (int list)

(*left and right projections of the product *)
projl : listPair -> int list 
projr: listPair -> int list 

(* Pervasive library functions for = and "-", these are monomorohic too *) 
eq : int -> int -> bool 
minus_one : int -> int


concat : int list -> int list -> int list  

parse_chunk : int list -> int -> int list -> listPair
(*example input output :  	
	(using alphabets for ease of explaining, we do not have support for chars so both the length and data are int) 
 	ac = [z] ->  n = 2 -> l = [a;b;c;d] => Pair ([z;a;b], [c;d])
    ac = [z] ->  n = 2 -> l = [a] => Exception 
    ac = [] ->  n = 2 -> l = [a;b;c;d] => Pair ([a;b], [c,d] *)
 

(*A p* function for a given p, also takes an accumulator, and an input list*)
p_star :  int list  -> int list -> (int list -> int -> int list -> listPair) -> listpair
(*example :
   (using alphabets for ease of explaining, we do not have support for chars so both the length and data are int) 
   (Star consumes input to the end)
   acc = [x;y] -> inpc = [3;a;b;c;2;d;e] -> p = (parse_chunk) => Pair ( [x;y;a;b;c;d;e] , [])
   acc = [x;y] -> inpc = [3;a;b;c;2;d;e;f] -> p = (parse_chunk) =>  Exception *)


parse_header : int -> int list ->  bool
(*matches the head of the input list to the given input int*)
(*example :
	2 -> [2;3;4;9] => true
	2 -> [3;3;4;9] => false
*)

parse_png : hd : int -> input : int list ->  pairList
(*example :
	hd = 88 -> input = [88;3;1;2;1;4;2;3;4;5] => Pair ([1;2;1;2;3;4;5], [])   					
	hd = 88 -> input = [888;3; 1;2;1; 4; 2;3;4;5] => Exception

	hd = 88 -> input = [88;3; 1;2;1; 4; 2;3;4;5;6] => Exception

	hd = 88 -> input = [88; 4;1;2;1;4; 2;3;4] => Pair ([1;2;1;4; 3;4], [])   					
*)		

(*other utilities for printing list and pairList and testing the implementation*)
string_of_intlist
string_of_pair 


$2. description mini_png.spec, describes relations and informal descriptions of specs using them
--------------------------------------------------------------------------
maps a list to the numeric value of its head, defined only for int list
relation Rhdn : int list  :-> (int) 

(* grounded/non-parametric instantiations of parametric relations (Rlen Rfst) , (Rlen Rsnd), (Rmem Rfst) etc.  *)

relation Rlenfst pairList :-> (int)
relation Rlensnd pairList :-> (int)
relation Rmemfst pairList :-> {(int)} 
relation Rmemsnd pairList :-> {(int)} 

(*we assume the specs about the Pervasive library functions and Exceptions*)
assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};
assume minus_one : n1 -> {vn | (vn) = (n1) -- (1)};

(*we also assume the spec regarding the first and the second projections for ease of understanding, derives from the relation defs straight-forwardly*)

assume projl : p0 -> {pl | Rmem (pl) = Rmemfst (p0)/\
						Rlen (pl) = Rlenfst(p0) };

assume projr : p1 -> {pr | Rmem (pr) = Rmemsnd (p1)/\
							Rlen (pr) = Rlensnd(p1) };


concat : int list -> int list -> {v | p1. output is the concatanation of the two inputs}

parse_chunk : (ac : int list) -> (n : int )-> 
							(l1 : int list)  -> 
								{v | p2 length (output) = length (ac) + (length parameter, n) 
									p3. length (input) = n + length (remaining_list)
									p4. The parsed output , i.e projl (v) contains only elements from ac or the input


p_star : (acc : int list)-> 
				(inpc : int list) ->
						
						P : (ac -> n ->  l1 -> 
								{v1 | 
									 p5. The numeric argument n passed to the higher order function p is same as the head of the input list	
									 p6. ( conjunction of other constraints from p2, p3, p4)
									  
								}
						) ->

							{v | p7. The members of the output are subset of members of the acc and the input
								 p8. Star consumes the input list till the end
							};

parse_header : (hd : int) -> ( inp :  int list) ->  {vhd | p9. iff "vhd = true"  then "head of the inp is equall to the given header hd"};

parse_png : hdr -> 
				{l1 | p10. head  of l1 = hdr }} -> 
					{v |   p11. Output is a sublist of the input 
							p12. The input is totally consumed and remaining list is empty
							
					};






$3. description safety properties of mini_png and what combination of p1-12 above capture these.
-----------------------------------------------------------------------------------------------
As discussed, one intuition about the safety property is along following lines
	 - The mandatory header is present at the beginning of the input
	 	1. Properties p9 and p10 verify this property, they check a much stronger property of equality of the header to a given header value
	 	2. p9 -> 54 in spec file, where (->) is "is defined by line number"
	 		p10 -> 57 
     

     - The length of each chunk matches its length field
     	
     	1. Properties p2, p3, p5, p6 capture this property.
     	2. p2 -> 33,  
     		p3 -> 34
     		p5 -> 42
     		p6 -> 43 - 45	
     	3. This property can be handled with the current hack of added simple arithmetic relations, however I need to handle few simple cases of comparing an arithmetic and set properties. 
     		(we checked a similar property in our last example file "small_png",which i sent earlier).

     	4. However, the following property cannot be checked by the Catalyst (with the simple arithmetic support)
     	we cannot say a property like the following for p_star, 
     		"sum of the "length fields" of all the chunks = length of the output for p_star + the len of acc"
		ex- consider p_star again,
		let us assume
		acc = [x;y] -> inpc = [3;a;b;c;2;d;e] -> p = (parse_chunk) => out = [x;y;a;b;c;d;e]
		Now, a stronger property could be 
		len (out) = 3 + 2 + len (acc), where 3 and 2 are length fields of the chunks which are parsed

		Checking such a property will require a more fundamental extension of theory of relations in catalyst with the theory of arithmetic in say Liquid Haskell.

		5. This property seems plausible to be checked by "Bounded Refinement Types"[1].

     - The output is a faithful representation of the input 
     	1. Properties p4, p7, p8 ,p11, p12 capture these properties
     		p4 -> 25
     		p7-p8 -> 49-51 
     		p11 -> 58
     		p12 -> 59
