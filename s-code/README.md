#Scheme interpreter and  S-Code compiler
Time-stamp: <2022-01-21 16:37:14 gorbag>

Given the number of scheme interpreters (free even) available, you're probably wondering why I'm writing one too.

First, the Scheme-79 chip (and future chips) execute S-Code as "machine code" and thus the scheme compiler has to be
able to produce S-Code. As far as I can tell, only the [MIT-Gnu Scheme](https://www.gnu.org/software/mit-scheme/)
distribution produces something called S-Code. This is somewhat problematic for our purposes however, for the following
reasons:

* The source code is mostly in 'C', and our project is in common-lisp (I don't use C except for drivers and such) making
  modification of that code difficult. AND this is supposed to be a contribution to the lisp community after all. At
  least it isn't [C++](https://en.wikiquote.org/wiki/Alan_Kay) (Keep in mind that the architecture I envision while PIM
  will still be based on Kay's notion of [OOP](https://www.purl.org/stefan_ram/pub/doc_kay_oop_en).
* The pedigree of the MIT-Scheme system starts with a version of the Scheme language newer than the one the Scheme-79
  chip could possibly have been based on (see Background of [Scheme
  R5RS](https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_2.html)). So the most likely version of
  Scheme immediately available to the authors was one based on [Scheme78](https://dspace.mit.edu/handle/1721.1/6283)
  with GLS's [Rabbit compiler](https://dspace.mit.edu/handle/1721.1/6913). It is likely this would run on the Lisp
  Machine, however I've only been able to locate code for Scheme and Rabbit that would run under MacLisp (on the PDP-10)
  See [here](https://github.com/PDP-10/its/tree/master/src/scheme). It also implies the S-Code used by the MIT-Scheme
  system may or may not be (directly) related to the S-Code GLS and company used for Scheme-79!
* Because the Scheme-79 chip only implements a subset of Scheme, any modern S-Code compiler would generate code that
  would not immediately be able to run anyway and would require translation. That may well require translation back to
  the Scheme source code and regeneration!
* Because our intent is to support the newer Scheme-86 version of the chip as well (as well as further improved future
  versions I design for higher degrees of distribution) it makes sense we have our own version of the compiler we can
  adjust as needed for the requirements of each of these implementations. In fact, as our eventual intent is to build a
  Lisp that is Scheme-like (but not standard Scheme) to support Hewitt Actors on a PIM-type architecture, we have all
  the more reason to support our own, possibly unique, interpreter and compiler.
  
## Plan
The original MacLisp version of Scheme and Rabbit from ITS/PDP-10 code will be used to frame an initial version in
common-lisp, and modified as needed to support the Scheme systems we will be building on FPGAs. At this time (1/21/22),
we have only hand-compiled code based on the AIM about the chip, as appears in test-2.


### Test Sets:

(None yet)

Filename | Description
-------- | -----------

####1-21-22 Bradford W. Miller

Just a gleam in the eye
