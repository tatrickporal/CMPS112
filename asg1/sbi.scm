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

(define (parse_command command)
	(when (not (null? (car command)))
		  (printf " command = ~s~n" (car command))
	)
)

(define (parse_arguments argument)
	(when (not (null? (cadr argument)))
		  (printf "argument thi = ~s~n" (cadr argument))
	) (when (null? (cadr argument))
		  (printf "WHAT THE FUCK  = ~s~n" )
	) 
)

(define (parse_line line)
	(when (not (null? line))
		  (parse_command (car line))
		  (parse_arguments (car line))
	)
)

(define (write-program-by-line filename program)
    (map 
    	(lambda (line) 
    		(parse_line  (cdr line ))
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