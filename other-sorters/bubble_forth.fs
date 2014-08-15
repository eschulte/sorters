#! /usr/sbin/gforth
\ adapted from http://rosettacode.org/wiki/Bubble_Sort#Forth
\ by Eric Schulte

argc @ 1- constant len
variable lst len cells allot

: bubble
    dup 1 do
        2dup
        i - cells bounds do
            i 2@ < if i 2@ swap i 2! then
        cell +loop
    loop 2drop ;

: arg-to-number ( adrr u -- ) 0 rot rot 0 rot rot >number 2drop drop ;

: args> ( r l -- )
    do next-arg arg-to-number i ! cell +loop ;

: >stdout ( r l -- )
    do i @ . cell +loop cr ;

lst dup len cells + swap 2dup args> lst len bubble >stdout bye
