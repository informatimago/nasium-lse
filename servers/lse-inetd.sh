#!/bin/sh
export TERM=dumb
export LSE_TELNET=T
export LSE_PAGER=NIL
export LSE_ROOT=/srv/lse/files/
cd $LSE_ROOT
exec /srv/lse/bin/lse \
    --mode-moderne \
    --sans-bip \
    --fleches-ascii \
    --entree-comme-xoff \
    --afficher-sans-accent \
    --accepter-minuscules \
    --affichage-mixte
#--- 
    --afficher-en-majuscules \
