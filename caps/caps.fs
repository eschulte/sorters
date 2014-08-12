#! /usr/sbin/gforth

512 constant max-line
32  constant ascii-space

create line-buffer max-line 2 + allot

: open-input ( addr u -- ) r/o open-file throw ;
: close-input ( addr -- ) close-file throw ;

: check-args ( addr u -- )
    2dup 0= swap 0= and if ." USAGE: caps input-file output-file" cr bye then ;

: cap-line-buffer ( u -- )
    line-buffer + line-buffer do
        i    c@ ascii-space =
        i 1+ c@ 96 >
        and if
            i 1+ c@ 32 - i 1+ c!
        then
    loop ;

: print-caps ( fd-in -- )
    begin
        dup line-buffer max-line rot read-line throw
        over 0 > if over cap-line-buffer then
    while
        line-buffer swap type cr
    repeat
    drop ;

next-arg check-args open-input print-caps close-input bye
