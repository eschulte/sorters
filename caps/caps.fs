#! /usr/sbin/gforth
( Capitalize the contents of /tmp/lorem.in and write to /tmp/lorem.out )
512 constant max-line
32  constant ascii-space

0 Value fd-in
0 Value fd-out

create line-buffer max-line 2 + allot

: open-input ( addr u -- )  r/o open-file throw to fd-in ;
: open-output ( addr u -- )  w/o create-file throw to fd-out ;

: ?ascii-space ( char -- bool ) 32 = ;
: ?lower-case ( char -- bool ) 96 > ;

: cap-line-buffer ( u -- )
    line-buffer + line-buffer do
        i    c@ ?ascii-space
        i 1+ c@ ?lower-case
        and if
            i 1+ c@ 32 - i 1+ c!
        then
    loop ;

: caps ( -- )
    begin
        line-buffer max-line fd-in read-line throw
        over 0 > if over cap-line-buffer then
    while
        line-buffer swap fd-out write-line throw
    repeat ;

s" /tmp/lorem.in" open-input
s" /tmp/lorem.out" open-output
caps
fd-in close-file throw
fd-out close-file throw
bye
