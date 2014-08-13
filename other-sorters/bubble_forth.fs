#! /usr/sbin/gforth
\ adapted from http://rosettacode.org/wiki/Bubble_Sort#Forth
\ by Eric Schulte

argc @ 1- constant len
variable lst len cells allot

: bubble { addr cnt -- }
    cnt 1 do
        addr cnt i - cells bounds do
            i 2@ < if i 2@ swap i 2! then
        cell +loop
    loop ;

: arg-to-number ( adrr u -- ) 0 rot rot 0 rot rot >number 2drop drop ;

: args> ( r l -- )
    do next-arg arg-to-number i ! cell +loop ;

: >stdout ( r l -- )
    do i @ . cell +loop cr ;

lst dup len cells + swap 2dup args> lst len bubble >stdout bye
