(* Control.Print.printDepth := 100;   *)

datatype ('a, 'b) heterolist = NIL | ::: of 'a * ('b, 'a) heterolist;

infixr 5 :::

(* val build4 = fn : 'a * 'b * 'a * 'b -> ('a, 'b) heterolist *)
fun build4 (a, b, c, d) = a ::: b ::: c ::: d ::: NIL;

(* val unzip = fn : ('a, 'b) heterolist -> 'a list * 'b list *)
fun unzip NIL = ([], [])
    | unzip (a ::: NIL) = ([a], [])
    | unzip (a ::: b ::: rest) =
    let
      val (ar, br) = unzip rest
    in
      (a :: ar, b :: br)
    end;

(* val zip = fn : 'a list * 'b list -> ('a, 'b) heterolist *)

exception Empty;

fun zip (xs, ys) =
    if length xs <> length ys then
        raise Empty
    else
        case (xs, ys) of
             ([], _) => NIL
           | (_, []) => NIL
           | (x::xs', y::ys') => x ::: y ::: zip (xs', ys');


