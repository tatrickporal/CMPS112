(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> 
                   let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))



    let rec cmp' list1 list2 = match (list1,list2) with
        | list1, []      -> 1 (*First is larger so return a 1*)
        | [], list2      -> -1 (*Second is larger so return a -1*)
        | [],[]          -> 0 (*They are equal in size so return a 0*)
        | car1::cdr1, car2::cdr2 ->
            let flag = cmp' cdr1 cdr2 in
                if(flag = 0 && car1 != car2) then
                    if(car1<car2) then -1
                    else 1
                else flag


        let trim list =
        let rec trim' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = trim' cdr
                 in  match car, cdr' with
                     | 0, [] -> []
                     | car, cdr' -> car::cdr'
                     in trim' list


    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

        let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = car1 - car2 - carry
          in if(diff >= 0) then diff :: sub' cdr1 cdr2 0
             else (diff + radix) :: sub' cdr1 cdr2 1 
         

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else let flag = cmp' value1 value2 in 
            if flag < 0 
            then let sign = (if neg1 = Pos then Neg else Pos) in 
            Bigint (sign, sub' value2 value1 0)
            else Bigint (neg1, sub' value1 value2 0)
    
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let flag = cmp' value1 value2 in 
            if flag < 0 
            then let sign = (if neg1 = Pos then Neg else Pos) in 
            Bigint (sign, sub' value2 value1 0)
            else Bigint (neg1, sub' value1 value2 0)
        else zero

    let two_times num =  add' num num 0   

   

    let rec mul' list1 list2 p2 = 
    if (cmp' p2 list1) = 1 
        then [], list1
    else let product, remainder  = 
            div' list1 (two_times list2) (two_times p2) in 
        if (cmp' remainder p2) < 0  then product, remainder
            else (add' product list2 0), (trim (sub' remainder p2 0)) 
    
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
    if neg1 = neg2 
        then let answer, _ = mul' value1 value2 [1] in Bigint(Pos, trim(answer))
    else 
        let answer,_ = mul' value1 value2 [1] in Bigint(Neg,trim(answer))

     let rec div' list1 list2 p2 = 
    if (cmp' list1 list2) < 0 
        then [], list1
    else let product, remainder  = 
            mul' list1 (two_times list2) (two_times p2) in 
        if (cmp' remainder list2) < 0  then product, remainder
            else (add' product p2 0), (trim (sub' remainder list2 0)) 


    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
    if neg1 = neg2 
        then let answer, _ = div' value1 value2 [1] in Bigint(Pos, trim(answer))
    else 
        let answer, _ = div' value1 value2 [1] in Bigint(Neg,trim(answer))

    let rem = add

    let pow = add

end

