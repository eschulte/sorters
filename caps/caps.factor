USING: kernel unicode.categories unicode.case io.files io.encodings.utf8 prettyprint ;
IN: caps

: caps ( -- ) "/tmp/lorem.txt" utf8 file-contents >title . ;

MAIN: caps
