(* $Id: lazystream.ml,v 353.2 2005-05-17 19:32:12-07 - - $ *)

let (!!) = Lazy.force

let prtf = Printf.printf

(* stream and lazy stuff *)

type 'a stream = End | Stream of 'a * 'a stream Lazy.t

let (@::) car cdr = Stream (car, cdr)

let head stream = match stream with
    | End -> invalid_arg "head"
    | Stream (car, _) -> car

let tail stream = match stream with
    | End -> invalid_arg "tail"
    | Stream (_, cdr) -> !!cdr

let rec take n stream = match n, stream with
    | 0, _ when n <= 0 -> End
    | _, End -> End
    | _, Stream (car, cdr) -> Stream (car, lazy (take (n - 1) !!cdr))

let rec list_of_stream stream = match stream with
    | End -> []
    | Stream (car, cdr) -> car :: list_of_stream !!cdr

let rec iter fn stream = match stream with
    | End -> ()
    | Stream (car, cdr) -> (fn car; iter fn !!cdr)

let rec iter2 fn stream1 stream2 = match stream1, stream2 with
    | End, _ -> ()
    | _, End -> ()
    | Stream (car1, cdr1), Stream (car2, cdr2)
             -> (fn car1 car2; iter2 fn !!cdr1 !!cdr2)

let rec map2 fn stream1 stream2 = match stream1, stream2 with
    | End, _ -> End
    | _, End -> End
    | Stream (car1, cdr1), Stream (car2, cdr2)
             -> Stream (fn car1 car2, lazy (map2 fn !!cdr1 !!cdr2))

(* stuff that uses streams and Nums *)

let rec range head limit =
    if head > limit
    then End
    else let next = head + 1
         in  Stream (head, lazy (range next limit))

let naturals = range 0 max_int

let fac n =
    let rec fac' n m = match n with
        | 0 -> m
        | n -> fac' (n - 1) (n * m)
    in  if n < 0 then invalid_arg "fac"
                 else fac' n 1

let printfac n = prtf "%d! = %d\n" n (fac n)

let printfacs n = iter printfac (take n naturals)

(* let fib = 0 : 1 : map2 (+) fib (tail fib) *)

let fibstream =
    let rec stream0 = Stream (0, stream1)
        and stream1 = lazy (Stream (1, stream2))
        and stream2 = lazy (map2 (+) stream0 !!stream1)
    in  stream0

let printfib n nfib = prtf "fib(%d) = %d\n" n nfib

let printfibs n = iter2 printfib naturals (take n fibstream)

