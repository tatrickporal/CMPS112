(* $Id: lazythunk.ml,v 361.1 2006-03-02 18:48:23-08 - - $ *)

open Printf

(* re-implementation of module Lazy *)

type 'a promise =
    | Value of 'a
    | Excep of exn
    | Delay of (unit -> 'a)

type 'a thunk = 'a promise ref

let force thunk = match !thunk with
    | Value value -> value
    | Excep excep -> raise excep
    | Delay delay -> (try  let value = delay ()
                           in  (thunk := Value value; value)
                      with excep -> (thunk := Excep excep;
                                     raise excep))

let (!!) = force

(* stream and lazy stuff *)

type 'a stream = End | Stream of 'a * 'a stream thunk

exception End_stream

let (@::) car cdr = Stream (car, cdr)

let head stream = match stream with
    | End -> raise End_stream
    | Stream (car, _) -> car

let tail stream = match stream with
    | End -> raise End_stream
    | Stream (_, cdr) -> !!cdr

let rec take n stream = match n, stream with
    | _, End -> End
    | n, _ when n <= 0 -> End
    | _, Stream (car, cdr) ->
          Stream (car, ref (Delay (fun () -> take (n - 1) !!cdr)))

let rec list_of_stream stream = match stream with
    | End -> []
    | Stream (car, cdr) -> car :: list_of_stream !!cdr

let rec drop n stream = match n, stream with
    | _, End -> End
    | n, _ when n <= 0 -> stream
    | _, Stream (car, cdr) -> drop (n - 1) !!cdr

let rec iter fn stream = match stream with
    | End -> ()
    | Stream (car, cdr) -> (fn car; iter fn !!cdr)

let rec iter2 fn stream1 stream2 = match stream1, stream2 with
    | End, _ -> ()
    | _, End -> ()
    | Stream (car1, cdr1), Stream (car2, cdr2)
          -> (fn car1 car2; iter2 fn !!cdr1 !!cdr2)

let rec iter3 fn stream1 stream2 stream3 =
    match stream1, stream2, stream3 with
    | End, _, _ -> ()
    | _, End, _ -> ()
    | _, _, End -> ()
    | Stream (car1, cdr1), Stream (car2, cdr2), Stream (car3, cdr3)
          -> (fn car1 car2 car3; iter3 fn !!cdr1 !!cdr2 !!cdr3)

let rec map2 fn stream1 stream2 = match stream1, stream2 with
    | End, _ -> End
    | _, End -> End
    | Stream (car1, cdr1), Stream (car2, cdr2)
          -> Stream (fn car1 car2,
                     ref (Delay (fun () -> map2 fn !!cdr1 !!cdr2)))

(* stuff that uses streams and Nums *)

let rec range head limit =
    if head > limit
    then End
    else let next = head + 1
         in  Stream (head, ref (Delay (fun () -> range next limit)))

let naturals = range 0 max_int

let fac n =
    let rec fac' n m = match n with
        | 0 -> m
        | n -> fac' (n - 1) (n * m)
    in  if n < 0 then invalid_arg "fac"
                 else fac' n 1

let printfac n = printf "%2d! = %10d\n" n (fac n)

let printfacs n = iter printfac (take n naturals)

(* lazy let fib = 0 : 1 : map2 (+) fib (tail fib) *)

let fibstream =
    let rec stream0 = Stream (0, stream1)
        and stream1 = ref (Delay (fun () -> Stream (1, stream2)))
        and stream2 = ref (Delay (fun () -> map2 (+) stream0 !!stream1))
    in  stream0

let printfib n nfib nfib' =
    printf "fib(%3d) = %11d, %20.15f\n"
           n nfib (float_of_int nfib /. float_of_int nfib')

let printfibs n = iter3 printfib naturals
                                 (take n fibstream)
                                 (take n (drop 1 fibstream))

