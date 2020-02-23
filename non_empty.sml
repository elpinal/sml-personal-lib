signature NON_EMPTY = sig
  type 'a t

  val make : 'a -> 'a list -> 'a t
  val uncons : 'a t -> 'a * 'a list
  val to_list : 'a t -> 'a list
end

structure NonEmpty :> NON_EMPTY = struct
  type 'a t = 'a list

  exception Unreachable

  fun make x xs = x :: xs
  fun uncons (x :: xs) = (x, xs)
    | uncons _         = raise Unreachable

  fun to_list xs = xs
end

signature NON_EMPTY = sig
  include NON_EMPTY

  val from_list : 'a list -> 'a t option
  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_left1 : ('a -> 'a -> 'a) -> 'a t -> 'a
  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

structure NonEmpty :> NON_EMPTY = struct
  open NonEmpty

  fun from_list []        = NONE
    | from_list (x :: xs) = SOME(make x xs)

  fun singleton x = make x []
  fun cons x xs = make x (to_list xs)
  fun head xs = #1 (uncons xs)

  fun map f xs =
  let val (y, ys) = uncons xs in
    make (f y) (List.map f ys)
  end

  fun fold_left1 f a =
  let
    val (x, xs) = uncons a
  in
    List.foldl (fn (y, acc) => f acc y) x xs
  end

  fun fold_left f init xs =
  let val (y, ys) = uncons xs in
    List.foldl (fn (x, acc) => f acc x) (f init y) ys
  end
end

type 'a non_empty = 'a NonEmpty.t
