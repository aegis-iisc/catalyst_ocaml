let rec list_length l = match l with 
| [] -> 0
| x :: xs -> list_length xs + 1