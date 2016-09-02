# Makefile for CLISP module set regexp

CC = gcc -mno-cygwin
CPPFLAGS = -I/usr/local/include
CFLAGS = -O2
INCLUDES= ..
MODPREP = ../modprep.fas
REGEX_O = regex.o
REGEX_H = regex.h
ALLOCA_H = alloca.h

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = regexi.o $(REGEX_O)  link.sh Makefile README regexp.lisp regexp.texinfo regexp.dvi preload.lisp
distribdir =

all : regexp.fas regexi.o $(REGEX_O) 

regexp.fas : regexp.lisp
	$(CLISP) -c regexp.lisp

regexi.o : regexi.m.c $(REGEX_H) $(INCLUDES)/clisp.h config.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c regexi.m.c -o regexi.o

regexi.m.c : regexi.c $(MODPREP)
	$(CLISP) $(MODPREP) regexi.c

regex.o : regex.c regex.h $(ALLOCA_H)
	$(CC) $(CPPFLAGS) $(CFLAGS) -I. -I$(INCLUDES) -DHAVE_CONFIG_H -c regex.c

alloca.o : alloca.c $(ALLOCA_H)
	$(CC) $(CPPFLAGS) $(CFLAGS) -I. -I$(INCLUDES) -DHAVE_CONFIG_H -c alloca.c

alloca.h : alloca_.h
	cp alloca_.h alloca.h-t
	mv alloca.h-t alloca.h

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a alloca.h
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

distclean : clean
	rm -f *.dvi

force :

