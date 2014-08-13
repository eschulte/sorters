#! /usr/sbin/gforth
\ adapted from http://rosettacode.org/wiki/Quick_Sort#Forth
\ by Eric Schulte
\ 
\ TODO: weird errors for some short lists

argc @ 1- cells constant length
variable lst length 1+ allot

: cells+ cells + ;

: mid ( l r -- mid ) over - cell / 2/ cells + ;

\ : verbose-mid ( l r -- mid ) 2dup over - cell / 2/ cells + dup . dup @ ." holds " . ." between " . ." and " . cr ;

: exchange ( addr1 addr2 -- ) dup @ >r over @ swap ! r> swap ! ;

: partition ( l r -- l r l2 r2 )
    2dup mid @ >r ( r: pivot )
    2dup begin
        swap begin dup @ r@ < while  1 cells+ repeat \ bottom up
        swap begin dup @ r@ > while -1 cells+ repeat \ top down
        2dup <= if 2dup exchange >r 1 cells+ r> -1 cells+ then
    2dup > until r> drop ;

: qsort ( l r -- )
    partition swap rot
    2dup < if recurse else 2drop then
    2dup < if recurse else 2drop then ;

: arg-to-number ( adrr u -- ) 0 rot rot 0 rot rot >number 2drop drop ;

: args> ( r l -- )
    do next-arg arg-to-number i ! cell +loop ;

: >stdout ( r l -- )
    do i @ . cell +loop cr ;

\ hack for bug on small lists
: 1pad -1 over ! 1+ ;

lst 1pad dup length + swap 2dup args> 2dup swap qsort >stdout bye
