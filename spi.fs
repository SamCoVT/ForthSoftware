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
\ : spi.mosiHI via.portb c@  $02 or   via.portb c! ;
\ : spi.mosiLO via.portb c@  $FD and  via.portb c! ;
: spi.clkHI  via.portb c@  $01 or   via.portb c! ;
: spi.clkLO  via.portb c@  $FE and  via.portb c! ;
\ : spi.clock  spi.clkHI spi.clkLO ;
: spi.miso?  via.portb c@  $80 and ;

\ Faster words in assembly
assembler-wordlist >order
: spi.clock  [ via.portb inc via.portb dec ] ; always-native
: spi.mosihi [ $02 lda.#  via.portb ora   via.portb sta ] ; allow-native
: spi.mosilo [ $FD lda.#  via.portb and.  via.portb sta ] ; allow-native
previous

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
    spi.ssLO
    $95 0 0 0 0 0 sd.cmd  $FF >spi ( Toss first result )
    $FF >spi \ 1 extra idle byte with SS low.
    spi.ssHI $FF >spi
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


: sd.readsectors  ( addr numsectors LBA.d )
   rot 
   0 ?do
\      ( debug ) ." i is " i . cr
      2dup  i s>d d+  ( compute the current LBA sector to read )
\      ( debug ) ." sector is " 2dup ud. 
      \ Send command 17
      spi.sslo
      0 -rot ( CRC)  dsplit ( sector# )  #17 sd.cmd sd.r1 drop
\      cr ." CMD17 sent - response = " . cr \ Remove drop above to use this line.
      sd.waittoken drop
\      ." Data token is " . cr \ Remove drop above to use this line.
      2 pick #512 i * + ( Calculate starting address for this 512 byte chunk)
\      ( debug ) ." address is " dup .
      dup #512 + swap ( Calculate ending memory address )
      do
         spi> i !
      loop
     \ cr
      spi> drop spi> drop ( Discard CRC )
 \     ." CRC is " spi> . spi> . cr \ Comment out above line to use this line.

      $FF >spi \ 1 extra idle byte with SS low.
      spi.ssHI $FF >spi  \ 1 extra idle byte with SS high.
   loop
   2drop ( Remove LBA address from return stack )
   drop ( starting memory address )
;


\ \\\\\\\\\\\\
\ FAT32 support
hex
\ Tali2 needs some extra double words.
: d=  ( d1 d2 -- f )
   rot = -rot = and ;
: d0=   ( d -- f ) 0= swap 0= and ;
: d0<>  ( d -- f ) or ;
: d2*   ( d -- f ) 2dup d+ ;
: d<    ( d -- f ) rot 2dup = if ( use LSBs ) 2drop <
   else ( use MSBs ) 2swap 2drop > then ;

: d0!  ( addr -- )  ( store double 0 at address )
   0. rot 2! ; allow-native
: d+!  ( d1 addr -- )  ( Add d1 to double at addr )
   dup >r 2@ d+ r> 2! ;
: d1+!  ( addr -- )  ( Add 1 to double at addr )
   1. rot d+! ;

\ Bring in the assembler for these words.
assembler-wordlist >order

: dlshift  ( d u -- d )  ( double shift left )
   0 ?do
      \ Tali uses NUXI format for doubles.
      [ clc
      2 rol.zx 3 rol.zx \ Most significant cell
      0 rol.zx 1 rol.zx \ Least significant cell
      ]
   loop ;

: drshift  ( d u -- d )  ( double right left )
   0 ?do
      \ Tali uses NUXI format for doubles.
      [ clc
      1 ror.zx 0 ror.zx \ Least significant cell
      3 ror.zx 2 ror.zx \ Most significant cell
      ]
   loop ;

previous \ remove the assembler wordlist

\ Tali's 2@ doesn't do things in the order we need.
\ This reads a 32-bit little-endian value from memory.
: 32bit@  ( addr -- d )   dup @ swap 2 + @ ;

\ fat variables
create sectorbuff 200 allot \ 512 bytes ( 200 hex) for buffer
2variable sector# \ The disk sector currently in the buffer

2variable partition_lba_begin
2variable fat_begin_lba
2variable cluster_begin_lba
variable sectors_per_cluster
2variable root_dir_first_cluster

\ A word for making offsets
: offset  ( u "name" -- ) ( addr1 -- addr2 )
   ( Save an offset when created. )
   ( When used, add the saved offset to an address )
   create ,  does> @ + ;

\ Offsets for the MBR
1BE offset >parttable
4 offset >parttype
8 offset >partlba
16 constant /partentry
\ Offsets for the Volume ID
0D offset >sect/clust
0E offset >reservedsect
24 offset >sect/fat
2C offset >rootclust
\ Offsets for directory entries
00 offset >filename
0B offset >attrib
14 offset >clustHI
1A offset >clustLOW
1C offset >filesize

( Offsets into file control structure.                    )
( A "fileid" is address of one of these structures and is )
( used for both files and directories.                    )

( reusing "00 offset >filename" < 11 bytes > )
( reusing "0B offset >attrib" for fam < 1 byte > )
0C offset >firstcluster   (   4 bytes )
10 offset >currentcluster (   4 bytes )
14 offset >currentsector  (   4 bytes )
18 offset >fileposition   (   4 bytes )
( reusing "1C offset >filesize" < 4 bytes > )
20 offset >linestart      (   4 bytes )
24 offset >linebuffaddr   (   2 bytes ) ( 0x26 bytes total )

1C constant /dirinfo      ( Bytes per dirinfo - only up   )
                          ( through >fileposition is used )
26 constant /fileinfo     ( Bytes per fileinfo structure )

: newfileid ( -- fileid )
   here /fileinfo allot ( Allocate memory )
   dup /fileinfo 0 fill ( Fill with zeroes ) ;

create working_dir /dirinfo allot

: finfo  ( fileid -- )  ( fileid info for debugging )
   cr ." FILEID: "          dup u.
   cr ." filename: "        dup >filename 0B type
   cr ." access mode: "     dup >attrib c@ u.
   cr ." first cluster: "   dup >firstcluster 2@ ud.
   cr ." current cluster: " dup >currentcluster 2@ ud.
   cr ." current sector: "  dup >currentsector 2@ ud.
   cr ." file position: "   dup >fileposition 2@ ud.
   cr ." file size: "       dup >filesize 2@ ud.
   cr ." linestart: "           >linestart 2@ ud. ;


\ FAT32 Words

\ Given the address of a partition entry in the partition table
\ determine if it's fat32 or not.
: fat32part?  ( addr_partentry -- f )
   >parttype c@   dup 0B = swap 0C =   or ; allow-native

\ Read sector (given as double) into sectorbuff.
: sector>buffer  ( sector#.d -- )
   ( See if we already have it in the buffer )
   2dup sector# 2@ d= if
      2drop exit ( already have this sector ) then
   2dup sector# 2! ( Remember this sector )
   sectorbuff -rot 1 -rot sd.readsectors ( Read the sector ) ;

: fat.init.vars  ( -- )
   \ Load the FAT Volume ID from the sector in
   \ partion_lba_begin and intialize the variables
   \ used to access the file system.
   \ Read in the FAT Volume ID.
   partition_lba_begin 2@  sector>buffer
   \ Get sectors per cluster.
   sectorbuff >sect/clust c@  sectors_per_cluster !
   \ Get start of fat.
   sectorbuff >reservedsect @ ( reserved sectors ) 0 ( make a double)
   partition_lba_begin 2@ d+   2dup fat_begin_lba 2!
   \ Get start of clusters
   sectorbuff >sect/fat 32bit@ ( sectors per fat )
   d2* ( two copies of FAT ) d+   cluster_begin_lba 2!
   ( todo : subtract two "clusters" from cluster_begin_lba to make it 0-based )
   (        instead of starting at 2 and requiring offsetting clusters by 2.  )
   \ Get root dir first cluster.
   sectorbuff >rootclust 32bit@
   \ Save it so we always have a copy.
   2dup root_dir_first_cluster 2!
   \ And start the working directory there.
   working_dir >firstcluster 2!
   \ Set the name of the root directory.
   s" /          "  working_dir >filename swap  move ;

: fat.init  ( -- )  ( Intialize the FAT values )
   0. sector>buffer \ get MBR
   \ Check the 4 partions for a FAT one.
   4 0 do
      sectorbuff >parttable \ Locate partition table
      i /partentry * + \ Pick a partition
      dup fat32part? if \ Check for fat32
         >partlba \ get starting LBA for this partition.
         32bit@   partition_lba_begin 2!
         ." Found FAT32 filesystem in partition " i .
         ."  at LBA " partition_lba_begin 2@ d.
      else
         drop
      then
   loop
   \ Make sure we found something.
   partition_lba_begin 2@ d0= if abort then
   \ Initialize all of the variables needed for operation.
   fat.init.vars
;


: log2  ( u -- u )  ( determine log2 for exact powers of 2 )
   0 swap begin dup 1 > while 2/ swap 1+ swap repeat drop ;

: cluster>sector  ( cluster# -- sector# )  \ Get starting sector for cluster
   2. d-
   \    sectors_per_cluster *
   \ We don't have 32-bit *, so fake it with left shifts.
   \ This works because sectors_per_cluster is always
   \ a power of 2.
   sectors_per_cluster @ log2 dlshift
   cluster_begin_lba 2@ d+ ;

: cluster>fatsector  ( cluster# -- sector# ) \ Get sector for fat.
   \ There are 128 FAT pointers in a sector.
   \  80 ( 128 ) /
   \ We don't have double division, so use right shifts.
   7 drshift
   
   \ Start at the fat LBA sector.
   fat_begin_lba 2@ d+
;

: nextcluster#  ( fileid -- cluster#.d )
   ( Determine next cluster )
   ( Load the correct fat sector into the buffer. )
   dup >currentcluster 2@   cluster>fatsector sector>buffer
   \ index it and get the new cluster number.
   >currentcluster 2@
   drop ( we don't need the MSB )
   80 mod \ Index into sector
   4 * ( 32-bit clusters )
   sectorbuff + \ Locate the next pointer
   32bit@ \ Read the cluster pointer.
;

: nextsector ( fileid -- ) ( Adjust fileid for next sector)
   ( See if we can just move to the next sector. )
   ( Calculate #sectors from beginning of data area. )
   dup >currentsector 2@  1. d+  cluster_begin_lba 2@ d-
   drop ( we don't need the upper bits )
   ( See if we're still in the same cluster )
   sectors_per_cluster @ mod if
      ( fileid ) 
      >currentsector d1+! ( Increment current sector )
   else
      ( We need to move to the next cluster. )
      dup >r nextcluster#
      2dup r@ >currentcluster 2! ( Update current cluster )
      cluster>sector ( Turn it into a sector )
      ( Save this as the current sector )
      r> >currentsector 2!
   then
;


\ Words for handling directories
: directory_offset  ( fileid -- offset_in_sectorbuffer )
   >fileposition 2@
   drop ( MSBs not needed )
   200 ( sector size ) mod ;

: direntry_invalid?  ( addr_direntry -- f )
   dup c@ E5 = \ See if it begins with E5
   \ Offset to the ATTRIB and see if it has all 4 lower bits
   \ set.
   swap >attrib c@ 0f and 0f =
   \ If either of these things are true, it's an invalid entry.
   or ;

: dir_next ( fileid -- )
   \ Move along one entry
   dup >r
   >fileposition 2@ 20. ( 32-bytes per directory entry ) d+
   2dup   r@ >fileposition 2!
   \ See if a new sector is needed.
   drop ( don't need MSBs for this )
   200 ( sector size ) mod 0= if
      r> nextsector  else r> drop  then ;

: move_to_cluster  ( fileid clusternum.d -- )
   \ Update the cluster and sector in the fileid structure.
   rot >r ( Save fileid )
   2dup   r@ >currentcluster 2!   cluster>sector
   2dup   r> >currentsector 2!
   \ Load the sector into the buffer.
   sector>buffer ;

: init_directory  ( fileid -- )
   \ Set all of the variables for walking a directory.
   dup  dup >firstcluster 2@ move_to_cluster
   \ Start the the beginning of the directory.
   >fileposition d0! ;  

: dirend?  ( addr -- f )  c@ 0= ;

: walkdirectory  ( xt fileid -- addr | 0)
   \ Run routine (given by xt) on every starting address of a
   \ directory entry in directory described by fileid.  The routine
   \ should be ( direntry_addr -- continueflag ) where continueflag
   \ is 1 if directory processing should continue. The return value
   \ of walkdirectory will either be the address of the directory
   \ entry it stopped on (in the sector buffer) or 0 if it reached
   \ the end.
   >r ( Save the fileid )
   begin
      \ Get the offset of the directory entry into the
      \ sectorbuffer.
      r@ directory_offset
      \ Turn it into an address in the sectorbuffer.
      sectorbuff +
      \ See if we have reached the end of the directory.
      dup dirend? if
         drop ( direntry ) r> 2drop ( xt and fileid)
         0 ( false ) exit then
      \ See if it's an unused entry.
      dup direntry_invalid? if
         \ Just put 1 on the stack to tell the routine to keep
         \ going.
         drop 1
      else
         \ It's a valid entry.  Call the processing routine on it.
         over execute
      then
      \ There should be a flag telling us if we should proceed to
      \ the next directory entry.
   while
         \ Proceed to the next entry
         r@ dir_next
   repeat
   \ If we made it here, we were told to stop.
   drop ( the xt )
   \ Leave the address of the directory entry on the stack.
   sectorbuff  r> directory_offset  +
;

: print-attrs ( direntry_addr -- ) ( Print file attributes )
   >attrib c@
   dup 01 and if [char] R emit else space then ( Read Only )
   dup 02 and if [char] H emit else space then ( Hidden    )
   dup 04 and if [char] S emit else space then ( System    )
   ( Skipping Volume ID )
   dup 10 and if [char] D emit else space then ( Directory )
       20 and if [char] A emit else space then ( Archive   )
   ;
   
: print-fileinfo ( direntry_addr -- true )
   \ Print filename, atrributes, and size.
   \ Always return true to continue on to the next file.
   cr
   dup >filename 08 type ." ." dup >filename 8 + 3 type
   space
   dup print-attrs  space
   >filesize 32bit@ 7 ud.r
   true ( Keep going ) ;

: ls ( -- ) ( List the working directory)
   cr ." FILENAME     ATTRIB   SIZE"
   working_dir init_directory
   ['] print-fileinfo working_dir walkdirectory drop ;

: bin ; ( Required by ANS, but we're ignoring it. )

( File access methods )
1 constant r/o  2 constant w/o  3 constant r/w

: findfile ( fileid 'findfile direntry_addr -- f )
( This word is used to help find a file in a directory  )
( with walkdirectory.  It needs the address of a fileid )
( structure with just the name filled in to be on the   )
( stack -- under the things walkdirectory needs.        )
   ( cr ." checking " dup 0B type cr .s ( DEBUG )
   0B ( length to compare )
   3 pick ( bring fileid struct address over )
   ( >filename -- but offset is zero)
   0B compare ;

: eof? ( fileid -- f ) ( return true if at end of file )
   dup >fileposition 2@ rot >filesize 2@ d= ;

( open-file - simplified for read-only access )
: open-file  ( c-addr u fam -- fileid ior )
   >r ( save fam )
   newfileid ( Allot memory for the file info )
   ( c-addr u fileid ) -rot 2 pick swap move ( filename )
   ( fileid ) r> over >attrib c! ( save fam )
   ( fileid ) working_dir  init_directory
   ( fileid ) ['] findfile working_dir walkdirectory
   ( fileid direntry|0 )
   ?dup 0= if
      cr ." Can't open file: " dup 0B type cr
      1 ( ior of 1 = Can't open file ) exit
   then
   ( fileid direntry ) swap >r ( save fileid )
   dup >clustLOW @ ( first cluster LSB )
   over >clustHI @ ( first cluster MSB )
   ( direntry firstcluster.d )
   2dup r@ >firstcluster 2! ( Save first cluster )
   2dup r@ >currentcluster 2! ( start at first cluster )
   ( direntry firstcluster.d )
   cluster>sector r@ >currentsector 2! ( starting sector )
   ( direntry ) dup >filesize 32bit@ r@ >filesize 2!
   ( direntry ) drop ( Done with direntry )
   r> ( bring back fileid structure )
   dup >fileposition d0!
   dup >linestart d0!
   0 ( no errors/exceptions )
;

: read-char  ( fileid -- c ) ( Note: does not check eof )
   dup >currentsector 2@ sector>buffer ( Get the sector )
   dup >fileposition 2@ ( Get the position in the file )
   200 ( 512 ) um/mod drop ( Get offset into buffer )
   sectorbuff + c@ swap ( Get the character )
   ( c fileid )
   ( Move to next character )
   dup >fileposition d1+! ( increment fileposition )
   ( Check to see if we've wrapped to a new sector )
   dup >fileposition 2@ 200 ( 512 ) um/mod drop 0= if
      nextsector ( uses fileid ) else drop ( the fileid )
   then
;

: eol?  ( c -- f )  ( Check for end of line )
   dup 0A = swap 0D = or ; ( CR or LF )
( Note: windows CR+LF will show up as two line endings. )
( This should be OK for parsing files. )

: read-line  ( caddr u1 fileid -- u2 flag ior )
   ( Save the start of the line )
   dup dup >fileposition 2@ rot >linestart 2!
   -rot
   ( fileid caddr u1 )
   ( attempt to read u1 characters )
   0 ?do
      ( cr ." DEBUG FINFO " over finfo )
      ( fileid caddr )
      ( Check for EOF )
      over dup eof?          ( cr ." DEBUG EOF " .s )       if
         2drop drop i 0 0 ( no error, just no more data )
         unloop exit then
      ( fileid caddr fileid )
      read-char              ( cr ." READ-CHAR:  " .s )
      2dup swap i + c! ( Save the char )
      ( Check for end of line )
      eol?                   ( cr ." DEBUG EOL? " .s )       if
         2drop i ( last char position ) 1 0
         unloop exit then
   loop
   ( We got all the requested chars without finding eol )
   drop
   ( fileid )
   ( Determine number of characters added )
   dup >fileposition rot >linestart d- d>s  1 0 ;

: close-file  ( fileid -- )
   ." close-file not implemented" drop ;
: delete-file  ( caddr u -- )
   ." delete-file not implemented" 2drop ;
: file-position  ( fileid -- ud ior )
   >fileposition 2@ ( Get current position )
   0 ( No error ) ;
: file-size  ( fileid -- ud ior )
   >filesize 2@ ( Get file size )
   0 ( No error ) ;
: resize-file ( ud fileid -- ior )
   ." resize-file not implemented" drop 2drop ;

: sectorff  ( #bytes.d fileid -- )
   ( Sector fast-forward - follow FAT chain      )
   ( to determine current cluster/sector values. )
   ( Assumes starting from beginning of file.    )
   >r ( Save fileid )
   ( Move to next sector for every 512 bytes )
   ( Divide count by 512  - using shifts     )
   9 drshift
   begin
      2dup d0<>
   while
         r@ nextsector ( Move to the next sector )
         1. d- ( Reduce the count )
   repeat
   r> drop 2drop ;

: reposition-file  ( ud fileid -- ior )
   >r ( Save the fileid )
   r@ >filesize 2@ 2over ( newpos filesize newpos )
   d< if ( Check bounds )
      ." Cannot resize beyond end of file"
      r> drop 2drop exit then
   ( New position is ok.  Save it. )
   2dup  r@ >fileposition 2!
   ( Rewind the file to the first sector )
   r@ >firstcluster 2@ ( Get the first cluster )
   2dup r@ >currentcluster 2! ( Make first cluster current )
   cluster>sector ( Determine sector for this cluster )
   r@ >currentsector 2! ( Make that the current )
   r> sectorff ( Follow the FAT chains to the right sector )
   0 ( IO Result - no error ) ;

: catfile  ( fileid -- )  ( Copy file contents to screen )
   >r ( Save fileid ) cr ( Start on the next line )
   begin r@ eof? 0= while r@ read-char emit repeat r> drop ;

: rewind  ( fileid -- )
   ( Move fileposition back to beginning of file )
   0. rot reposition-file drop ;



decimal
( Temporary strings for filenames )
80 constant stringsize ( 80 char strings )
2  constant #strings
create stringsbuff stringsize #strings * allot
variable whichstring
hex

: nextstring ( -- caddr ) ( Determine next string addr )
   whichstring @ 1+ #strings mod ( Determine which is next )
   dup whichstring ! ( Update the variable for later )
   stringsize * stringsbuff + ; ( Calculate address )

: " ( ccc" -- c-addr u ) ( Put a string into stringsbuff )
   [char] " parse
   stringsize min ( No more than 80 chars )
   nextstring ( Use the next available temporary string )
   swap 2dup 2>r cmove ( Copy into the current string )
   2r> ; ( c-addr u for result left on stack )

: dotindex ( caddr u -- u2 ) ( Find the . in a filename )
   ( Return 0 if not found - or filename starts with . )
   0 ?do
      dup c@ [char] . = if drop i unloop exit then 1+
   loop ( If we made it here, no . )
   drop 0 ;

: string>filename ( caddr u -- caddr2 u2 )
   ( Convert a regular filename string into diraddr format )
   ( "FNAME.EXT" becomes "FNAME   EXT"                     )
   nextstring dup 0B blank ( Start with blank filename )
   dup >r ( Save location for later )
   -rot                                 ( ^ dest src u )
   0C min ( No more than 12 chars for filename.ext )
   2dup dotindex ( See if there is a . in the filename )
   (                                 ^ dest src u udot )
   ?dup 0= if ( No . so copy whole filename )
      rot swap move ( Copy whole string )
   else
      ( Figure out how many chars in ext and save )
      swap over - 1- >r                 ( ^ dest src udot )
      2dup + 1+ >r ( addr after .         ^ dest src udot )
      >r over r> move              ( ^ dest )
      8 + ( Move to extension in destination )
      r> swap r> ( Bring back source after .   ^ srcext destext uext )
      move
   then
   r> ( Bring back original temp string ) 0B ;
      
: f" ( ccc" -- caddr u )
   ( Accept a filename and convert to dirinfo format )
   [char] " parse string>filename ;


: cd ( caddr u -- ) ( Change the working directory )
   string>filename drop
   working_dir init_directory ( Start at the beginning of dir )
   ( Address of dirname in dirinfo format is on the stack )
   ['] findfile working_dir walkdirectory
   ( working_dir_fileid direntry )
   ?dup 0= if
      cr ." Can't find directory: " dup 0B type cr exit
   then
   ( working_dir_fileid direntry )
   swap drop ( Get rid of working_dir_fileid for now )
   dup >clustLOW @ swap >clustHI @ ( Get starting cluster )
   2dup d0= if ( Check for zero - meaning use root dir )
      ( Replace with correct cluster for root dir )
      2drop root_dir_first_cluster 2@
   then
   ( directory_firstcluster.d )
   ( Save everything to move to the new directory )
   working_dir >firstcluster 2! ( Fill in starting cluster )
   working_dir init_directory ; ( Fill in all the rest     )

   


\ Testing
sd.init fat.init

\ : lcd.fs s" LCD     FS " ; ( A filename in direntry format )

\ f" LCD.FS" r/o open-file drop constant lcdfileid


decimal
create mybuff 80 allot
: catbyline
   >r
   begin
      mybuff 80 r@ read-line drop
   while
         mybuff swap cr type
   repeat
   r> drop ;


\ NEEDS WORK SECTION

variable current_fileid
0 current_fileid !

: source-id ( -- u) ( Modify source-id to return file-id if available ) 
   current_fileid @ ?dup if else source-id then ;

: included ( caddr u -- ) ( Open and interpret file )
   r/o open-file if exit then ( Make sure the file opens. )
   cr
   ( fileid )
   current_fileid @ >r   current_fileid ! ( save fileid for source-id )
   begin
      current_fileid @   mybuff 80 rot read-line drop
      ( numchars_read not_at_eof_flag )
   while
         ( numchars_read )
         dup mybuff swap type cr ( Type it to the screen. )
         mybuff swap evaluate ( Run it. )
   repeat
   r> current_fileid ! ( restore previous fileid )
;

: include ( "filename" -- ) ( prefix version of included )
   parse-name ( filename comes next - with no quotes )
   string>filename included ;

