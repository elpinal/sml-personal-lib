signature MAP_MIN = sig
  type 'a t
  type key

  val empty : 'a t
  val insert : key -> 'a -> 'a t -> 'a t

  val delete : key -> 'a t -> 'a t

  val is_empty : 'a t -> bool
  val lookup : key -> 'a t -> 'a option

  val fold_left : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

signature MAP = sig
  include MAP_MIN

  val singleton : key -> 'a -> 'a t
  val from_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list

  val size : 'a t -> int

  val alter : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val update : key -> ('a -> 'a option) -> 'a t -> 'a t
  val adjust : key -> ('a -> 'a) -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_with_key : (key -> 'a -> 'b) -> 'a t -> 'b t
  val app : ('a -> unit) -> 'a t -> unit
  val app_with_key : (key -> 'a -> unit) -> 'a t -> unit

  (* Right-biased *)
  val union : 'a t -> 'a t -> 'a t

  val union_with : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val intersection : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  exception Duplicate of key
  val disjoint_union : 'a t -> 'a t -> 'a t
end

functor MakeMap (X : MAP_MIN) :> MAP where type key = X.key = struct
  open X

  exception Duplicate of key

  fun singleton k v = insert k v empty

  fun from_list xs =
    List.foldl (fn ((k, v), m) => insert k v m) empty xs

  fun to_list m =
  let
    fun f acc k v = Queue.push acc (k, v)
  in
    Queue.to_list (fold_left f Queue.empty m)
  end

  fun size m =
  let
    fun f n _ _ = n + 1
  in
    fold_left f 0 m
  end

  fun alter k f m =
    case f (lookup k m) of
         NONE   => delete k m
       | SOME v => insert k v m

  fun update k f = alter k
    (fn NONE   => NONE
      | SOME v => f v)

  fun adjust k f = alter k
    (fn NONE   => NONE
      | SOME v => SOME (f v))

  fun map_with_key f m =
  let
    fun g acc k v = insert k (f k v) acc
  in
    fold_left g empty m
  end

  fun map f = map_with_key (fn _ => f)

  fun app_with_key f m =
  let
    fun g () k v = f k v
  in
    fold_left g () m
  end

  fun app f = app_with_key (fn _ => f)

  fun union x y =
  let
    fun f acc k v = insert k v acc
  in
    fold_left f x y
  end

  fun union_with g x y =
  let
    fun f acc k v =
      case lookup k x of
           NONE    => insert k v acc
         | SOME v' => insert k (g v' v) acc
  in
    fold_left f x y
  end

  fun disjoint_union x y =
  let
    fun f acc k v = alter k
      (fn NONE => SOME v
      | SOME _ => raise Duplicate(k))
      acc
  in
    fold_left f x y
  end

  fun intersection f x y =
  let
    fun g acc k v2 =
      case lookup k x of
           NONE    => acc
         | SOME v1 => insert k (f k v1 v2) acc
  in
    fold_left g empty y
  end
end
