signature NAT = sig
  type t

  datatype u
    = Zero
    | Succ of t

  val proj : t -> u

  val from_int : int -> t option
  val from_int_exn : int -> t

  val zero : t
  val one : t

  val succ : t -> t
  val pred_checked : t -> t

  val to_int : t -> int

  val eq : t * t -> bool
  val gt : t * t -> bool
  val lt : t * t -> bool
  val geq : t * t -> bool
  val leq : t * t -> bool

  val length_of_list : 'a list -> t
end

structure Nat :> NAT = struct
  type t = int

  datatype u
    = Zero
    | Succ of t

  fun proj n =
    if 0 < n
    then Succ(n - 1)
    else Zero

  fun from_int n =
    if 0 <= n
    then SOME n
    else NONE

  exception Negative of int

  fun from_int_exn n =
    case from_int n of
         NONE   => raise Negative(n)
       | SOME n => n

  val zero = 0
  val one = 1

  fun succ n = n + 1

  exception ZeroPred

  fun pred_checked n =
    case proj n of
         Succ n' => n'
       | Zero    => raise ZeroPred

  fun to_int n = n

  fun eq (x, y : t) = x = y
  fun gt (x, y : t) = x > y
  fun lt (x, y : t) = x < y
  fun geq (x, y : t) = x >= y
  fun leq (x, y : t) = x <= y

  fun length_of_list xs = List.length xs
end

type nat = Nat.t
