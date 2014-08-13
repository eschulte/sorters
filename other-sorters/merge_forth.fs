#! /usr/sbin/gforth
\ adapted from http://rosettacode.org/wiki/Merge_sort#Forth
\ by Eric Schulte

argc @ 1- constant len
variable lst len cells allot

: merge-step ( right mid left -- right mid+ left+ )
    over @ over @ < if
        over @ >r
        2dup - over dup cell+ rot move
        r> over !
        >r cell+ 2dup = if rdrop dup else r> then
    then cell+ ;
: merge ( right mid left -- right left )
    dup >r begin 2dup > while merge-step repeat 2drop r> ;

: mid ( l r -- mid ) over - 2/ cell negate and + ;

: mergesort ( right left -- right left )
    2dup cell+ <= if exit then
    swap 2dup mid recurse rot recurse merge ;

: sort ( addr len -- )  cells over + swap mergesort 2drop ;

: arg-to-number ( adrr u -- ) 0 rot rot 0 rot rot >number 2drop drop ;

: args> ( r l -- )
    do next-arg arg-to-number i ! cell +loop ;

: >stdout ( r l -- )
    do i @ . cell +loop cr ;

lst dup len cells + swap 2dup args> lst len sort >stdout bye
