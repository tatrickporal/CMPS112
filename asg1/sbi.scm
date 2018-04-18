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


(define (write-program-by-line filename program)

    (map (lambda (line) 
    		(decifer  (cdr line))
    	) program)
    ;;(decode program)
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
             	(program (readlist-from-inputfile sbprogfile)))
            	(write-program-by-line sbprogfile program))))

;;Custom Tables Start
;;Stores
(define *label-table* (make-hash))

;;Stores variables defined in SBIR files
(define *var-table* (make-hash))
(for-each
    (lambda (element) 
        (hash-set! *var-table* (car element) (cadr element)))
    `( 
       (e 2.718281828459045235360287471352662497757247093)
       (pi 3.141592653589793238462643383279502884197169399)
     )
)

(define *function-table* (make-hash))
(define (symbol-get key)
        (hash-ref *function-table* key))
(define (symbol-put! key value)
        (hash-set! *function-table* key value))
(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (-       ,-)
        (/       ,(lambda (x y) ( / x y)))
        (*       ,*)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)

     ))
;;End Custom Tables

(define (execute decode)
	(let ((execute  (car decode) ))
		( cond
			((= 1 (length (car decode)) )
				(printf "length 1 ~s~n" (car decode) )
			)
			((pair? (list (car decode)))
				(let ((operator (car execute)))
					(printf "~s" (list? (cadr execute)))
					(printf " ~s" ((symbol-get operator) (cadr execute) (caddr execute)) )
					
				)
			)

		
		)
	)
	
)

( define (decode statement state_len)
	
 (let ((command  (car statement) ))
 	;;(printf "~s, length = ~s~n" command (length (command)))
 	(cond
 		; if 
 		((eqv? command 'print)
 			(cond ((= 1 state_len) (printf "~n")) 
 				((= 2 state_len) 
 					(display (cadr statement))
 					(newline)
 				)
 				((= 3 state_len) 
 					(display (cadr statement))
 					(execute (cddr statement)) 
 					(newline)
 				)
 			)
 		)

 		((eqv? command 'let)
 			(cond 
 				(printf "is ~s~n" command)
 			)
 		)

 	)
 )
)


(define (decifer line)

 (when(not(null? line))
 	(let ((statement (car line)))
		
		(cond ((symbol? (car line)) (hash-set! (cadr line) line)) ;; if label put into label-table
			((= 3 (length statement))
				(decode  statement (length statement))
			)
			((= 2 (length statement))
				(decode  statement (length statement)) 
			)
			((= 1 (length statement))
				(decode  statement (length statement)) 
			)
		)

	)
 )
)




(main (vector->list (current-command-line-arguments)))