functor BinarySearchMap (X : ORDERED) = MakeMap (struct
  type key = X.t

  datatype 'a t
    = E
    | T of 'a t * key * 'a * 'a t

  val empty = E

  fun singleton k v = T(E, k, v, E)

  fun insert k1 v1 E                 = singleton k1 v1
    | insert k1 v1 (T(l, k2, v2, r)) =
        case X.compare (k1, k2) of
             LESS    => T(insert k1 v1 l, k2, v2, r)
           | EQUAL   => T(l, k1, v1, r)
           | GREATER => T(l, k2, v2, insert k1 v1 r)

  fun min_view E               = NONE
    | min_view (T(l, k, v, r)) =
        case min_view l of
             NONE              => SOME (k, v, r)
           | SOME (k0, v0, r0) => SOME (k0, v0, T(r0, k, v, r))

  fun delete k1 E = E
    | delete k1 (T(l, k2, v, r)) =
        case X.compare (k1, k2) of
             LESS    => T(delete k1 l, k2, v, r)
           | GREATER => T(l, k2, v, delete k1 r)
           | EQUAL   =>
               case (l, r) of
                    (E, E) => E
               (* | (_, E) => l *)
                  | (E, _) => r
                  | _      =>
                      case min_view r of
                           NONE              => l
                         | SOME (k0, v0, r') => T(l, k0, v0, r')

  fun is_empty E = true
    | is_empty _ = false

  fun lookup k1 E = NONE
    | lookup k1 (T(l, k2, v, r)) =
        case X.compare (k1, k2) of
             LESS    => lookup k1 l
           | EQUAL   => SOME v
           | GREATER => lookup k1 r

  fun fold_left f init E = init
    | fold_left f init (T(l, k, v, r)) =
        let
          val init = fold_left f init l
          val init = f init k v
          val init = fold_left f init r
        in
          init
        end
end)
