#!/bin/sh
export LC_ALL=en_US.UTF-8
export TERM=vt100
export LSE_TELNET=NIL
export LSE_PAGER=NIL
export LSE_ROOT=/srv/lse/files/
export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib
cd $LSE_ROOT
exec /srv/lse/bin/lse \
    --mode-ancien \
    --sans-bip \
    --fleches-unicode \
    --entree-comme-xoff \
    --afficher-sans-accent \
    --afficher-en-majuscules \
    --accepter-minuscules \
    --montrer-touches 
