signature DEQUE_MIN = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  val from_list : 'a list -> 'a t

  val push : 'a t -> 'a -> 'a t
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t

  val cons : 'a -> 'a t -> 'a t
end

signature DEQUE = sig
  include DEQUE_MIN

  val to_list : 'a t -> 'a list
end

(* Output-restricted. *)
structure Deque :> DEQUE_MIN = struct
  type 'a t = 'a list * 'a list

  val empty = ([], [])

  fun is_empty (xs, _) = null xs

  fun from_list xs = (xs, [])

  fun check ([], ys) = (rev ys, [])
    | check q        = q

  fun push (xs, ys) x = check (xs, x :: ys)

  fun head ([], _)      = NONE
    | head (x :: xs, _) = SOME x

  fun tail (x :: xs, ys) = check (xs, ys)
    | tail q             = q

  fun cons x (xs, ys) = (x :: xs, ys)
end

structure Deque :> DEQUE = struct
  open Deque

  fun to_list q =
    case head q of
         NONE   => []
       | SOME x => x :: to_list (tail q)
end
