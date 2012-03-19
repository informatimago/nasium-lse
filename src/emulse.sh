#!/bin/bash
oldfont='-dec-terminal-bold-r-normal--14-0-75-75-c-*-dec-dectech' 
font='-dec-terminal-bold-r-normal-*-14-*-*-*-*-*-dec-dectech'

bin="$(dirname $0)"
second=0

for arg ; do
    case "$arg" in
    --second)
        second=1
        ;;
    *)
        bin="$arg"
        ;;
    esac
done

if [ "$second" -ne 0 ] ; then
    bin="$bin/bin"
    save="`stty --save`"
    trap "echo '' ; echo Exiting... ; stty $save ; exit " SIGINT SIGTERM SIGQUIT

    # We reset the terminal to function as a LSE console.
    # The intr signal is sent upon CTRL-A  key (used by ATT() function).
    # The quit signal is sent upon ESC key key.
    # Start, stop and susp signals are not mapped.
    stty raw  \
        olcuc -ocrnl -onlcr \
        -echo -echoe -echok -echonl -echoprt -echoke -echoctl \
        intr   0x01 \
        start  0x00 \
        stop   0x00 \
        susp   0x00 \
        quit   0x1b \
        erase  0x00 \
        kill   0x00 \
        eof    0x00 \
        eol    0x00 \
        eol2   0x00 \
        rprnt  0x00 \
        werase 0x00 \
        lnext  0x00 

    echo  "$bin"/lse 
    "$bin"/lse 2> trace

    stty $save
else

    xterm \
        -bg '#000' \
        -fg '#5f2' \
        -fn $font \
        -T 'L.S.E. - Console no. 0' \
        +mb +vb +aw +rw -dc \
        -e "$0" --second "$bin" &

fi

exit 0

#### emulse.sh                        -- 2003-09-26 18:58:44 -- pascal   ####
