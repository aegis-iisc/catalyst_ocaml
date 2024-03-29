(** State *)

(* It is often beneficial to have some form of a mutable state, which can be
   implemented using handlers. Below is an example of a simple integer state. *)

effect Get: int
effect Set: int -> unit

(* The monad_state handler wraps the computation in a function and the state
   is passed around as the function input. The initial value of the state
   is given to the transformed computation as input. *)

let monad_state = handler
  | y -> (fun _ -> y)
  | effect Get k -> (fun s -> (continue k s) s)
  | effect (Set s') k -> (fun _ -> (continue k ()) s')
;;

let f = with monad_state handle
  let x = perform Get in
  perform (Set (2 * x));
  perform Get + 10
in
f 30
;;

(* We can improve it by giving the state a default initial value using the
   "finally" keyword. The value of the initial value is given to the handler
   as a parameter. By modifying the case for values we also return the result
   and the final state value instead of just the result. *)

let better_state initial = handler
  | y -> (fun s -> (y, s))
  | effect Get k -> (fun s -> (continue k s) s)
  | effect (Set s') k -> (fun _ -> (continue k ()) s')
  | finally f -> f initial
;;

with better_state 30 handle
  let x = perform Get in
  perform (Set (2 * x));
  perform Get + 10
;;
(** State *)

(* It is often beneficial to have some form of a mutable state, which can be
   implemented using handlers. Below is an example of a simple integer state. *)

effect Get: int
effect Set: int -> unit

(* The monad_state handler wraps the computation in a function and the state
   is passed around as the function input. The initial value of the state
   is given to the transformed computation as input. *)

let monad_state = handler
  | y -> (fun _ -> y)
  | effect Get k -> (fun s -> (continue k s) s)
  | effect (Set s') k -> (fun _ -> (continue k ()) s')
;;

let f = with monad_state handle
  let x = perform Get in
  perform (Set (2 * x));
  perform Get + 10
in
f 30
;;

(* We can improve it by giving the state a default initial value using the
   "finally" keyword. The value of the initial value is given to the handler
   as a parameter. By modifying the case for values we also return the result
   and the final state value instead of just the result. *)

let better_state initial = handler
  | y -> (fun s -> (y, s))
  | effect Get k -> (fun s -> (continue k s) s)
  | effect (Set s') k -> (fun _ -> (continue k ()) s')
  | finally f -> f initial
;;

with better_state 30 handle
  let x = perform Get in
  perform (Set (2 * x));
  perform Get + 10
;;
