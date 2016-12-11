CM.make "./sources.cm";
Main.run (1, 2);

(* let-polymorphism *)

val s = fn x => fn y => fn z => x z (y z);
val k = fn x => fn y => x;
s k k;
(* val it = fn : ('16 -> '16) *)

(* laziness *)

let val rec bot = bot in
  (fn x => 1) bot
end;
(* val it = 1 : int *)

let val rec bot = bot in
  case (bot, 2) of (x, y) => y
end;
(* val it = 2 : int *)

val if_ = fn b => fn x => fn y =>
  if b then x else y;
val rec fix = fn f => f (fix f);
fix (fn fib => fn x =>
  if_ (x <= 1) x (fib (x - 1) + fib (x - 2))) 6;
(* val it = 8 : int *)

(* circular programming *)
val rec map = fn f => fn l =>
  case l of [] => [] | h :: t => f h :: map f t;
val rec even = 0 :: map (fn x => x + 1) odd
and odd = map (fn x => x + 1) even;
(* val it = (0 :: 2 :: 4 :: 6 :: 8 :: ... , 1 :: 3 :: 5 :: 7 :: 9 :: ...) : int * list * int list *)

val rec zipWith = fn f => fn l1 => fn l2 =>
  case (l1, l2) of
       ([], _) => []
     | (_, []) => []
     | (x :: l1, y :: l2) => f x y :: zipWith f l1 l2;
val tail = fn x =>
  case x of h :: t => t;
val rec fibs = 0 :: 1 :: zipWith (fn x => fn y => x + y) fibs (tail fibs);
(* val it = 0 :: 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: 13 ... : int list *)

(* infinite loop *)
val rec x = y
and y = x;

