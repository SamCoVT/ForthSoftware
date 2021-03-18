\ PROGRAMMER  : Sam Colwell
\ FILE        : spi.fs
\ DATE        : 2020-03-11
\ DESCRIPTION : Routines for bit-bang SPI

\ These are being developed on mecrisp forth for KL25Z.
\ They will then be ported to TaliForth2 on 65C02.

\ Hardware specific words:
hex
: spi.setup
    \ MISO = A4
    \ MOSI = A5
    \ CLK  = D4
    \ SS   = A12
    \ Make the pins we want GPIO (PCR for pin 0x100)
    100 PORTD_PCR 4  cells + !
    100 PORTA_PCR 4  cells + !
    100 PORTA_PCR 5  cells + !
    100 PORTA_PCR #12 cells + !
    \ Set the output pin data before making pins outputs.
    1020 GPIOA_PDOR !  \ Bits 12 and 5 (MOSI and SS) high
    0    GPIOD_PDOR !  \ CLK low
    \ Turn on outputs.
    1020 GPIOA_PDDR !
    10   GPIOD_PDDR !    
;

: spi.sshi GPIOA_PDIR @  1000 or   GPIOA_PDOR ! ;
: spi.sslo GPIOA_PDIR @  1000 bic  GPIOA_PDOR ! ;

: spi.clkhi GPIOD_PDIR @  10 or   GPIOD_PDOR ! ;
: spi.clklo GPIOD_PDIR @  10 bic  GPIOD_PDOR ! ;
: spi.clock spi.clkhi spi.clklo ;

: spi.mosihi GPIOA_PDIR @  20 or   GPIOA_PDOR ! ;
: spi.mosilo GPIOA_PDIR @  20 bic  GPIOA_PDOR ! ;

: spi.miso? GPIOA_PDIR @  10 and ;

\ Generic SPI words:
: >spi> ( u -- u ) \ send/receive byte over SPI bus
    \ MSb first
    8 0 do
        1 lshift  spi.miso? if 1 + then
        dup $100 and if spi.mosihi else spi.mosilo then
        spi.clock
    loop
    \ Chop result to just byte recieved.
    $ff and ;

 : >spi ( u -- )   >spi> drop ;
 : spi> ( -- u )   FF >spi> ;

: sd.cmd ( crc7 argLSB arg arg argMSB cmd# -- resp )
    %01000000 or >spi  ( Send command )
    >spi >spi >spi >spi ( Send argument)
    >spi ( Send CRC - always 0x95)
    ( Wait for response )
    begin $ff >spi>  dup $80 and while drop repeat ;

: sd.cmd0 ( -- resp )
    $95 0 0 0 0 0 sd.cmd ;
: sd.cmd1 ( -- resp )
    0 0 0 0 0 1 sd.cmd ;
: sd.cmd8 ( -- resp )
   $87 $AA $01 00 00 08 sd.cmd ;

: sd.cmd55 ( -- )
         0 0 0 0 0 #55 sd.cmd . space
;
: sd.acmd41 ( -- resp )
      0 0 0 0 0 #55 sd.cmd . space
      0 0 0 0 40 #41 sd.cmd . cr
;


: sd.init ( -- )
    spi.setup
    spi.sshi 80 0 do spi.clock loop
    spi.sslo
    sd.cmd0  1 = if
        cr ." Command 0 accepted" cr
        ." Starting initialization (Command 1)."
        begin sd.cmd1  1 and while ." ." repeat
        ." COMPLETE" cr
        ." Sending Command 8: "
        5 0 do spi> . space loop cr
    then
\    begin
\        ." Send ACMD 41" cr
\        0 0 0 0 0 #55 sd.cmd drop
\        0 0 0 0 0 #41 sd.cmd
\    0= until
    
    
;

: readmbr
    \ Send command 17
    0 0 0 0 0 #17 sd.cmd
    cr ." CMD17 sent - response =" . cr
    begin $ff >spi>  dup $ff = while  drop repeat
    ." Data token is " . cr
    ." Data is " cr
    512 0 do
        i 32 mod 0= if cr then
        $ff >spi> . space
    loop
    cr
    $ff >spi> ." CRC is " . cr ;




    
    


    