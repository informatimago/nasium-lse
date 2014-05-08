
#-(and) "

- un sifflement par instruction LIRE.

- valeur numérique assignée à la variable à la lecture de X-OFF ou
  SPC.

- valeur chaîne assignée à la variable à la lecture de X-OFF ou RET
  sur Mitra-15 (mais RET inclus).




standard

read line by line.  The standard doesn't preclude reading character by
character, but there's no conforming way to configure it.


swank

read line by line (but possibly also character by character with
support from emacs).


unix

read character by character, with full support for ESC, C-a, C-s etc.


reading strings:

read characters until RET or C-s or ESC or string full.
ESC is interrupt.
string full is an error condition, 
RET is included in the string, C-s not.  (It's virtual RET/C-s, real RET can be mapped to C-s).


reading numbers:

skip spaces as many as you want.
read characters until RET or C-s or spaces or ESC or buffer full.
ESC is interrupt.
string full is an error condition, 
parse the buffer and return the number or signal an error.

Now the problem is that reading can be buffered already, so we may get a (unfinished) line.


Interrupts (ESC, C-a) are processed by the low level I/O.
"

