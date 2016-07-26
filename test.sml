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

let
  val rec even = fn n => 
    if n <= 0 then true
    else if odd (n - 1) then false
    else true
  and odd = fn n =>
    if even n then false
    else true
in even 10 end
(* val it = true : bool *)


let
  val rec zipWith = fn f => fn l1 => fn l2 =>
    case l1 of
         [] => []
       | x :: l1 =>
           (case l2 of
                 [] => []
               | y :: l2 => f x y :: zipWith f l1 l2)
in zipWith end
(* val it = fn : (('_16 -> ('_15 -> '_14)) -> ('_16 list -> ('_15 list -> '_14 * list))) *)
