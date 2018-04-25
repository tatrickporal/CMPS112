(* $Id: lazythunk-p4.ml,v 353.1 2005-05-17 19:25:45-07 - - $ *)

open Printf

(* re-implementation of module Lazy *)

#load "camlp4o.cma";;
let lazyexpand _ expr = "(ref (Delay (fun () -> (" ^ expr ^ "))))";;
Quotation.add "lazy" (Quotation.ExStr lazyexpand);;

type 'a suspension =
    | Value of 'a
    | Excep of exn
    | Delay of (unit -> 'a)

type 'a thunk = 'a suspension ref

exception Thunk_cycle

let rec force thunk = match !thunk with
    | Delay delay -> (thunk := Excep Thunk_cycle;
                      try  let value = delay ()
                           in  (thunk := Value value; value)
                      with excep -> (thunk := Excep excep; raise excep))
    | Value value -> value
    | Excep excep -> raise excep

let (!?) = force

(* stream and lazy stuff *)

type 'a stream = End | Stream of 'a * 'a stream thunk

exception End_stream

let (@::) hd tl = Stream (hd, tl)

let head = function
    | End            -> raise End_stream
    | Stream (hd, _) -> hd

let tail = function
    | End            -> raise End_stream
    | Stream (_, tl) -> !?tl

let rec take n stream = match n, stream with
    | _, End             -> End
    | n, _ when n <= 0   -> End
    | _, Stream (hd, tl) -> Stream (hd, <:lazy< (take (n - 1) !?tl)>>)

let rec drop n stream = match n, stream with
    | _, End             -> End
    | n, _ when n <= 0   -> stream
    | _, Stream (hd, tl) -> drop (n - 1) !?tl

let rec list_of_stream = function
    | End             -> []
    | Stream (hd, tl) -> hd :: list_of_stream !?tl

let rec iter fn1 = function
    | End             -> ()
    | Stream (hd, tl) -> (fn1 hd; iter fn1 !?tl)

let rec iter2 fn2 = function
    | End, _ -> ()
    | _, End -> ()
    | Stream (hd1, tl1), Stream (hd2, tl2)
             -> (fn2 hd1 hd2; iter2 fn2 !?tl1 !?tl2)

let rec iter3 fn3 = function
    | End, _, _ -> ()
    | _, End, _ -> ()
    | _, _, End -> ()
    | Stream (hd1, tl1), Stream (hd2, tl2), Stream (hd3, tl3)
                -> (fn3 hd1 hd2 hd3; iter3 fn3 !?tl1 !?tl2 !?tl3)

let rec zip fn = function
    | End, _ -> End
    | _, End -> End
    | Stream (hd1, tl1), Stream (hd2, tl2)
             -> Stream (fn hd1 hd2, <:lazy< (zip fn !?tl1 !?tl2)>>)

(* stuff that uses streams and Nums *)

let rec range head limit =
    if head > limit
    then End
    else let next = head + 1
         in  Stream (head, <:lazy< (range next limit)>>)

let naturals = range 0 max_int

let fac n =
    let rec fac' n m = match n with
        | 0 -> m
        | n -> fac' (n - 1) (n * m)
    in  if n < 0 then invalid_arg "fac"
                 else fac' n 1

let printfac n = printf "%d! = %d\n" n (fac n)

let printfacs n = iter printfac (take n naturals)

(* let fib = 0 : 1 : zip (+) fib (tail fib) *)

let fibstream =
    let rec fibstream0 = Stream (0, fibstream1)
        and fibstream1 = <:lazy< (Stream (1, fibstream2))>>
        and fibstream2 = <:lazy< (zip (+) fibstream0 !?fibstream1)>>
    in  fibstream0

let printfib n nfib nfib' =
    printf "fib(%3d) = %11d, %20.15f\n"
           n nfib (float_of_int nfib /. float_of_int nfib')

let printfibs n = iter3 printfib naturals
                                 (take n fibstream)
                                 (take n (drop 1 fibstream))

