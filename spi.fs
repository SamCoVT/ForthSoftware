\ PROGRAMMER  : Sam Colwell
\ FILE        : spi.fs
\ DATE        : 2020-03-11
\ DESCRIPTION : Routines for bit-bang SPI

\ These are being developed on mecrisp forth for KL25Z.
\ They will then be ported to TaliForth2 on 65C02.

\ Hardware specific words:
\ KL25Z with mecrisp
hex
: spi.init
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



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ TaliForth2 on SBC words for SD card over SPI
$7F00 constant via.portb
$7F02 constant via.ddrb
: spi.init
    \ Using VIA Port B for SPI
    \ Bit 0 = CLK
    \ Bit 1 = MOSI
    \ Bit 2 = Slave Select
    \ Bit 7 = MISO (using bit 7 for faster reads in assembly)
    $04 via.portb c! \ Slave select high
    $07 via.ddrb  c! \ Make CLK, MOSI, and SS outputs
;

: spi.ssHI   via.portb c@  $04 or   via.portb c! ;
: spi.ssLO   via.portb c@  $FB and  via.portb c! ;
: spi.mosiHI via.portb c@  $02 or   via.portb c! ;
: spi.mosiLO via.portb c@  $FD and  via.portb c! ;
: spi.clkHI  via.portb c@  $01 or   via.portb c! ;
: spi.clkLO  via.portb c@  $FE and  via.portb c! ;
: spi.clock  spi.clkHI spi.clkLO ;
: spi.miso?  via.portb c@  $80 and ;




\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
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

: >spi ( u -- )   >spi> drop ; always-native
: spi> ( -- u )   $FF >spi> ; always-native

: spi.clear ( -- ) #520 0 do spi> drop loop ;

: sd.r1 ( -- u )
    \ Wait for a response.
    \ Try up to 1000 times
    1000 0 do  spi>
        dup $80 and  0= if unloop exit then  drop
    loop
    ." SD CARD TIMEOUT "
    $ff ( put a dummy value on stack )
;

: sd.r7 ( -- d true | u false )
    ( flag is 0 for R1 response with error, true for full [32-bit] R7 response )
    sd.r1 dup 1 > if false exit then
    drop ( the R1 response )
    spi> 8 lshift spi> or  ( upper half )
    spi> 8 lshift spi> or  ( lower half )
    swap ( make DOUBLE order)
    true ;
    

: .sd.r1 ( u -- )
    dup $40 and if ." PARAMETER ERROR " then
    dup $20 and if ." ADDRESS ERROR " then
    dup $10 and if ." ERASE SEQUENCE ERROR " then
    dup $08 and if ." COM CRC ERROR " then
    dup $04 and if ." ILLEGAL COMMAND " then
    dup $02 and if ." ERASE RESET " then
        $01 and if ." IDLE " then ;
     
 
: sd.cmd ( crc7 argLSB arg arg argMSB cmd# -- resp )
    %01000000 or >spi  ( Send command )
    >spi >spi >spi >spi ( Send argument)
    >spi ( Send CRC - always 0x95)
    ;

: sd.cmd0 ( -- resp )
    spi.ssLO
    $95 0 0 0 0 0 sd.cmd  sd.r1
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi ; \ 1 extra idle byte with SS high.


: sd.cmd1 ( -- resp )
    spi.ssLO
    0 0 0 0 0 1 sd.cmd  sd.r1
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi ; \ 1 extra idle byte with SS high.

: sd.cmd8 ( -- resp f ) ( f=0 indicates failure and R1 response, F=true indicates R7 response)
    spi.ssLO
    $87 $AA $01 00 00 08 sd.cmd  sd.r7
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi ; \ 1 extra idle byte with SS high.

: sd.cmd58 ( -- resp f ) ( f=0 indicates failure and R1 response, F=true indicates R7 response)
    spi.ssLO
    0 0 0 0 0 #58 sd.cmd  sd.r7 \ Technicaly an R3 response, but format is identical.
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi ; \ 1 extra idle byte with SS high.


: sd.cmd55 ( -- )
    spi.ssLO
    0 0 0 0 0 #55 sd.cmd  sd.r1
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi ; \ 1 extra idle byte with SS high.


: sd.acmd41 ( -- resp )
    sd.cmd55 drop
    spi.ssLO
    0 0 0 0 $40 #41 sd.cmd sd.r1
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi ; \ 1 extra idle byte with SS high.



: sd.init ( -- )
   spi.init
   spi.sshi 80 0 do spi.clock loop
   4 0 do
      sd.cmd0  1 = if
         cr ." Command 0 accepted" cr leave
      then
      \ Abort if it never worked.
      i 3 = if unloop exit then
   loop
   
   ." Sending Command 8: "
   4 0 do sd.cmd8 if leave then
      i 3 = if ." ERROR" unloop exit then
   loop
   ud.
   
   ." Sending AMCD41: "
   #200 0 do
      ." ."
      sd.acmd41
      0= if ." SUCCESS" cr leave then
   loop
;

: split
   dup $ff and  swap 8 rshift ;
: dsplit ( du -- lsb b b msb )  ( Split a double word into bytes )
   swap split ( work with LSB first)
   rot split ( MSB ends up on TOS ) ;
   
: sd.waittoken ( -- u ) ( Wait for data token)
   #200 0
   do spi> dup
      $ff <> if leave then
      drop
      i #199 = if
         ." READ TIMEOUT" unloop unloop
         $ff ( Return $ff as that's an invalid token) exit
      then
   loop ;


: readsectors  ( addr numsectors LBA.d )
   rot 
   0 ?do
      ( debug ) ." i is " i . cr
      2dup  i s>d d+  ( compute the current LBA sector to read )
      ( debug ) ." sector is " 2dup ud. 
      \ Send command 17
      spi.sslo
      0 -rot ( CRC)  dsplit ( sector# )  #17 sd.cmd sd.r1
      cr ." CMD17 sent - response = " . cr
      sd.waittoken
      ." Data token is " . cr
      2 pick #512 i * + ( Calculate starting address for this 512 byte chunk)
      ( debug ) ." address is " dup .
      dup #512 + swap ( Calculate ending memory address )
      do
         spi> i !
      loop
      cr
      ." CRC is " spi> . spi> . cr

      $FF >spi \ 1 extra idle byte with SS low.
      spi.ssHI $FF >spi  \ 1 extra idle byte with SS high.
   loop
   2drop ( Remove LBA address from return stack )
   drop ( starting memory address )
;






 
    

