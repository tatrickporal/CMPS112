#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " , *run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
             		program) ))
)
;; object tests
(define tests `(
   (,boolean?   boolean?)
   (,char?      char?)
   (,complex?   complex?)
   (,integer?   integer?)
   (,list?      list?)
   (,number?    number?)
   (,pair?      pair?)
   (,path?      path?)
   (,procedure? procedure?)
   (,rational?  rational?)
   (,real?      real?)
   (,string?    string?)
   (,symbol?    symbol?)
   (,vector?    vector?)
))

;;display lists and their values
(define (show label it)
    (display it)
    (newline)
)

;;hashtables of functions


(define label (make-hash))
(define functions (make-hash))
(define variables (make-hash))

(for-each
    (lambda (item) (hash-set! functions (car item) (cadr item)))
    `(
   	  (print, (lambda (message) (printf "~s~n" message)))
      (+ ,(lambda (x y) (+ x y)))
      (- ,(lambda (x y) (- x y)))
      (* ,(lambda (x y) (* x y)))
      ;;(let,(lambda (var_name x) (let var_name x)))
      (vec ,(make-vector 10 0.0))
     )
)


(define (decifer line)
	(cond 
		(not(null? (car temp)) (printf " ~s~n" (caar line)))
		(else (printf "this please"))
	)	
)

(define (write-program-by-line filename program)
    (map 
    	(lambda (line) 
    		(decifer  (cddr line))
    	)  
    program)
)
    
(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          (else 'other)))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
             	(program (readlist-from-inputfile sbprogfile)))
            	(write-program-by-line sbprogfile program))))
		
(main (vector->list (current-command-line-arguments)))