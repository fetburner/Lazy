(* let-polymorphism *)

CM.make "./sources.cm";
Main.run (1, 2);

let
  val s = fn x => fn y => fn z => x z (y z)
  val k = fn x => fn y => x
in s k k end
(* val it = fn : ('_16 -> '_16) *)

(* laziness *)

let val rec bot = bot in
  (fn x => 1) bot
end;
(* val it = 1 : int *)

let val rec bot = bot in
  case (bot, 2) of (x, y) => y
end;
(* val it = 2 : int *)

let
  val if_ = fn b => fn x => fn y =>
    if b then x else y
  val rec fix = fn f => f (fix f)
in
  fix (fn fib => fn x =>
    if_ (x <= 1) x (fib (x - 1) + fib (x - 2))) 6
end
(* val it = 8 : int *)

(* circular programming *)
let
  val rec map = fn f => fn l =>
    case l of [] => [] | h :: t => f h :: map f t
  val rec even = 0 :: map (fn x => x + 1) odd
  and odd = map (fn x => x + 1) even
in (even, odd) end
(* val it = (0 :: 2 :: 4 :: 6 :: 8 :: ... , 1 :: 3 :: 5 :: 7 :: 9 :: ...) : int * list * int list *)

let
  val rec zipWith = fn f => fn l1 => fn l2 =>
    case (l1, l2) of
         ([], x) => []
       | (x, []) => []
       | (x :: l1, y :: l2) => f x y :: zipWith f l1 l2
  val tail = fn x =>
    case x of h :: t => t
  val rec fibs = 0 :: 1 :: zipWith (fn x => fn y => x + y) fibs (tail fibs)
in fibs end
(* val it = 0 :: 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: 13 ... : int list *)
