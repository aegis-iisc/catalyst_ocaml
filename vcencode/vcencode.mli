val z3_log : string -> unit

type result = Success | Undef | Failure

val discharge : VerificationCondition.t -> result