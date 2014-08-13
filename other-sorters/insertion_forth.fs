#! /usr/sbin/gforth
\ adapted from http://rosettacode.org/wiki/Insertion_sort#Forth
\ by Eric Schulte

argc @ 1- constant len
variable lst len cells allot

: insertion ( start end -- start )
    dup @ >r ( r: v )                   \ v = a[i]
    begin
        2dup <                          \ j>0
    while
            r@ over -1 cells + @ <      \ a[j-1] > v
        while
                -1 cells +              \ j--
                dup @ over cell + !     \ a[j] = a[j-1]
        repeat then
    r> swap ! ;                         \ a[j] = v

: sort ( array len -- )
    1 ?do dup i cells + insertion loop drop ;

: arg-to-number ( adrr u -- ) 0 rot rot 0 rot rot >number 2drop drop ;

: args> ( r l -- )
    do next-arg arg-to-number i ! cell +loop ;

: >stdout ( r l -- )
    do i @ . cell +loop cr ;

lst dup len cells + swap 2dup args> lst len sort >stdout bye
