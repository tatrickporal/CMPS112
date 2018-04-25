#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/mzscheme/bin/mzscheme -qC
;; $Id: lazylist.scm,v 353.2 2012-02-21 19:26:32-08 - - $

;;
;; This program shows factorial, the ``hello world'' of functional
;; programming.  It also shows lazy evaluation.  Note that the
;; ``positiveintegers'' described below contain a lazy list of many
;; positive integers.
;;

(define (void arg) '())

(define BIGNUM 100000000000000000000000000000000000000000000000000)

(define (fac n)
    (letrec
        ((fac2 (lambda (n m)
                (if (<= n 1) m (fac2 (- n 1) (* n m))))))
        (fac2 n 1)
))

(define (printfac n)
    (printf "~s! = ~s~n" n (fac n))
)

(define (.. first last)
    (if (> first last) '()
        (delay (cons first (.. (+ first 1) last)))
))

(define positiveintegers (.. 1 BIGNUM))

(define (take n lazylist)
    (if (<= n 0) '()
        (let ((forcedlist (force lazylist)))
             (cons (car forcedlist)
                   (take (- n 1) (cdr forcedlist)))
)))

(define (lazytake n lazylist)
    (if (<= n 0) '()
        (let ((forcedlist (force lazylist)))
             (delay (cons (car forcedlist)
                          (lazytake (- n 1) (cdr forcedlist))))
)))

(define (lazymap fn lazylist)
    (if (null? lazylist) '()
        (let ((forcedlist (force lazylist)))
             (cons (fn (car forcedlist))
                   (lazymap fn (cdr forcedlist)))
)))

(define (main argvl)
    (printf "BIGNUM = ~s~n" BIGNUM)
    (lazymap printfac (lazytake 50 positiveintegers))
    #f
)

