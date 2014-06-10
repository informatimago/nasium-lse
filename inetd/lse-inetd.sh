#!/bin/sh
export TERM=dumb
export LSE_TELNET=T
export LSE_PAGER=NIL
export LSE_ROOT=/srv/lse/files/
cd $LSE_ROOT
exec /srv/lse/bin/lse \
    --mode-moderne \
    --fleches-ascii \
    --entree-comme-xoff \
    --afficher-sans-accent \
    --afficher-en-majuscules \
    --accepter-minuscules \
    --montrer-touches 
