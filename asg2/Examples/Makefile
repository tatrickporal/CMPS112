# $Id: Makefile,v 1.1 2011-04-22 20:30:48-07 - - $

SOURCES    = \
	ackermann.ml \
	argv.ml \
	exponent.ml \
	factorial.ml \
	fibonacci.ml \
	helloworld.ml \
	hof.ml \
	mergesort.ml \
	msorttailrec.ml \
	ncat.ml \
	qsort.ml \
	reverse.ml

BINS    = ${SOURCES:.ml=.b}
TYPES   = ${SOURCES:.ml=.types}
LISS    = ${SOURCES} Makefile
LISTING = Listing.ps

all: ${BINS}

types: ${TYPES}

%.types: %.ml
	ocamlc -i $< >$@

%.b: %.ml
	cid $<
	ocamlc $< -o $@

lis: ${SOURCES}
	mkpspdf ${LISTING} 

