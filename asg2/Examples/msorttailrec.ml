(* $Id: msorttailrec.ml,v 341.4 2014-11-17 14:05:54-08 - - $ *)

(*
* A better version of mergesort.
* Uses tail recursion for split and merge, but msort is O(log n) deep.
* Note that in this case split reverses the list.
* Merge then reverses it again giving properly sorted final output,
* but msort has to alternate between less and not less on alternate
* levels of the recursion.
* The functions split and merge should probably be nested inside
* msort, but they are left external for easier debugging.
* Also given here explicitly are higher order functions.
*)

let rec foldl fn ident list = match list with
    | []       -> ident
    | car::cdr -> foldl fn (fn ident car) cdr

let rec foldr fn ident list = match list with
    | []       -> ident
    | car::cdr -> fn car (foldr fn ident cdr)

let cons car cdr = car::cdr

let swap fn x y = fn y x

let revcat = foldl (swap cons)

let reverse = revcat []

let un boolfn x y = not (boolfn x y)


let merge less list1 list2 =
    let rec merge' in1 in2 out = match (in1, in2) with
            | ([], [])   -> out
            | ([], list) -> revcat out list
            | (list, []) -> revcat out list
            | (car1::cdr1 as list1), (car2::cdr2 as list2)
                         -> if less car2 car1
                            then merge' cdr1 list2 (car1::out)
                            else merge' list1 cdr2 (car2::out)
    in  merge' list1 list2 []

let split list =
    let rec split' list out1 out2 = match list with
        | []              -> (out1, out2)
        | [car]           -> (car::out1, out2)
        | car::cadr::cddr -> split' cddr (car::out1) (cadr::out2)
    in split' list [] []

let msort less list =
    let rec msort' less list = match list with
        | []            -> []
        | [car] as list -> list
        | list          -> let (list1, list2) = split list
                           in  merge less (msort' (un less) list1)
                                          (msort' (un less) list2)
    in msort' less list

let msortlt = msort (<)

;;

msortlt [33;11;-10;12;44;202;8;66];;

