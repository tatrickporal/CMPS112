(* $Id: hof.ml,v 361.7 2008-03-06 18:14:34-08 - - $ *)

let compose f g x = f (g x)

let compose2 f g x y = f (g x y)

let swap f x y = f y x

let even x = x mod 2 = 0

let odd = compose not even

let cons car cdr = car::cdr

let rec foldl fn unit list = match list with
    | [] -> unit
    | car::cdr -> foldl fn (fn unit car) cdr

let rec foldr fn unit list = match list with
    | [] -> unit
    | car::cdr -> fn car (foldr fn unit cdr)

let rec foldl2 fn unit list1 list2 = match list1, list2 with
    | [], [] -> unit
    | car1::cdr1, car2::cdr2 -> foldl2 fn (fn unit car1 car2) cdr1 cdr2
    | _, _ -> raise (Invalid_argument "foldl2")

let rec foldr2 fn unit list1 list2 = match list1, list2 with
    | [], [] -> unit
    | car1::cdr1, car2::cdr2 -> fn car1 car2 (foldr2 fn unit cdr1 cdr2)
    | _, _ -> raise (Invalid_argument "foldl2")

let sum = foldl (+) 0

let lengthrec list =
    let rec lengthrec' list' len' = match list' with
        | [] -> len'
        | _::cdr -> lengthrec' cdr (len' + 1)
    in  lengthrec' list 0

let lengthf list = foldl (fun len _ -> len + 1) 0 list

let reverserec list =
    let rec reverserec' list' revlist = match list' with
        | [] -> revlist
        | car::cdr -> reverserec' cdr (car::revlist)
    in  reverserec' list []

let reversef list = foldl (swap cons) [] list

let filterrec test list =
    let rec filterrec' list' = match list' with
        | [] -> []
        | car::cdr when test car -> car :: (filterrec' cdr)
        | _::cdr -> filterrec' cdr
    in  filterrec' list

let filterf test =
    foldr (fun car cdr -> if test car then car::cdr else cdr) []

let maprec fn list =
    let rec maprec' list' = match list' with
        | [] -> []
        | car::cdr -> (fn car) :: maprec' cdr
    in  maprec' list

let mapf fn = foldr (compose cons fn) []

let mapf2 fn = foldr2 (compose2 cons fn) []

let innerprodl = foldl2 (fun sum val1 val2 -> sum + val1 * val2) 0

let innerprodr = foldr2 (compose2 (+) ( * )) 0

let rec memberrec elt list = match list with
    | [] -> false
    | car::_ when car = elt -> true
    | _::cdr -> memberrec elt cdr

let memberf elt = foldl (fun car cdr -> car = elt || cdr) false

let zipf list1 list2 = mapf2 (fun a b -> a, b) list1 list2

let qsort (<?) list =
    let (>=?) x y = not (x <? y) in
    let rec qsort' list' = match list' with
        | [] -> []
        | car::cdr ->
            qsort' (filterf (swap (<?) car) cdr)
          @ [car]
          @ qsort' (filterf (swap (>=?) car) cdr)
    in  qsort' list
    
