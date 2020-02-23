infix |>
infixr $

structure Std :> sig
  exception TODO
  exception NotYetImplemented of string
  exception Unreachable

  val |> : 'a * ('a -> 'b) -> 'b
  val $ : ('a -> 'b) * 'a -> 'b
end = struct
  exception TODO
  exception NotYetImplemented of string
  exception Unreachable

  fun x |> f = f x
  fun f $ x = f x
end
