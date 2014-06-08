#!/bin/sh
export TERM=dumb
export LSE_TELNET=T
export LSE_ROOT=/home/lse/files/
cd $LSE_ROOT
exec /home/lse/bin/lse \
    --mode-moderne \
    --fleches-ascii \
    --entree-comme-xoff \
    --afficher-sans-accent \
    --afficher-en-majuscules \
    --accepter-minuscules \
    --montrer-touches 

