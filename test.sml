(* let-polymorphism *)

let
  val s = fn x => fn y => fn z => x z (y z)
  val k = fn x => fn y => x
in s k k end
(* val it = fn : ('_16 -> '_16) *)

(* laziness *)

let val rec bot = fn x => bot x in
  (fn x => 1) (bot ())
end;
(* val it = 1 : int *)

let val rec bot = fn x => bot x in
  case (bot (), 2) of (x, y) => y
end;
(* val it = 2 : int *)

let
  val if_ = fn b => fn x => fn y =>
    if b then x else y
  val rec fib = fn x =>
    if x <= 1 then x else fib (x - 1) + fib (x - 2)
in fib 6 end
(* val it = 8 : int *)

let
  val rec nat = fn n => n :: nat (n + 1)
  val rec map = fn f => fn l =>
    case l of [] => [] | h :: t => f h :: map f t
  val rec take = fn n => fn l =>
    if n <= 0 then []
    else case l of [] => [] | h :: t => h :: take (n - 1) t
in take 4 (map (fn x => 2 * x + 1) (nat 0)) end
(* val it = (1 :: (3 :: (5 :: (7 :: [])))) : int list *)
