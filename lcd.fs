( LCD Routines - SamCo - 2018-11-15 ) hex
: lcd-init ( -- ) 38 7FC0 c! 0E 7FC0 c! ;
: lcd-busy ( -- )         ( Wait until LCD is not busy )
    begin 7FC0 c@ 80 and while repeat ;
: lcd-cmd ( cmd -- )      ( Send an LCD command )
    lcd-busy 7FC0 c! ; allow-native
: lcd-char ( char -- )    ( Send an LCD character )
    lcd-busy 7FC1 c! ; allow-native
: lcd-type ( addr n -- )  ( Send a string to the LCD )
    0 ?do dup i + c@ lcd-char loop drop ;
: lcd." ( Make and send string ) postpone s" lcd-type ;
( Helper words )
: lcd-line1 ( -- ) 80 lcd-cmd ; allow-native
: lcd-line2 ( -- ) C0 lcd-cmd ; allow-native
: lcd-clear ( -- ) 01 lcd-cmd ; allow-native
decimal
