open Printf 
let file = "example.dat"

let () = let ic = open_in file in 
                try 
                let s = really_input_string ic (in_channel_length ic) in
                (*let line = input_line ic in*) 
                Printf.printf "%s" s;
                (*print_endline line;*)
                flush stdout;
                close_in ic

                with 
                e -> close_in_noerr ic;
                raise e 
