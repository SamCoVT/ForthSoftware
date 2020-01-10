\ PROGRAMMER  : Sam Colwell
\ FILE        : file.fs
\ DATE        : 2020-01
\ DESCRIPTION : My second attempt at Compact Flash
\ and FAT32 support for my 65C02 Single Board Computer
\ using Tali Forth 2, implementing the ANS file word set.

\ Compact Flash card support using 8-bit interface

hex
7F40 constant cf.base
cf.base constant cf.data
cf.base 1+ constant cf.feature ( Error Register when read )
cf.base 2 + constant cf.sectorcount ( Config for SetFeatures command )
cf.base 3 + constant cf.lba ( low byte )
\ A note about the LBA address - it's 28 bits, but we can't use a
\ double in Tali to store it because the two 16-bit halves are in the
\ wrong order in Tali.  We CAN use two regular 16-bit cells because
\ they are little endian like the compact flash card expects for the
\ LBA #, but will have to transfer them separately.  I will likely
\ use a double and then split it to load the LBA.
cf.base 6 + constant cf.drivehead
( bit 6 : set to 1 = LBA mode )
cf.base 7 + constant cf.status ( Command Register when written )
( bit 7 : 1 = BUSY )
( bit 6 : 1 = RDY [ready] )
( bit 3 : 1 = DRQ [data request] data needs to be transferred )

: cf.busywait begin cf.status c@ 80 and 0= until ;

: cf.init ( -- ) ( Initilize the CF card in 8-bit IDE mode )
   \ Reset the CF card.
   04 cf.status c!
   cf.busywait
   \ Set feature 0x01 (8-bit data transfers)
   01 cf.feature c!
   EF cf.status c!
;

: cf.read ( addr numsectors LBA.d -- )
   ( Read numsectors starting at LBA [double] to addr )
   \ Treat LBA double as two 16-bit cells.
   \ MSB is on top.  We can write this as a 16-bit cell to
   \ cf.lba+2, however we need to make sure we don't screw
   \ up the Drive and LBA bits (in the MSB)
   EFFF and ( make sure Drive is zero )
   E000 or  ( make sure LBA and reserved bits are 1 )
   cf.lba 2 + ! ( store high word of LBA )
   cf.lba !     ( store  low word of LBA )
   \ stack is now just ( addr numsectors )
   \ Tell the CF card how many sectors we want.
   dup cf.sectorcount c!
   
   \ Send the command to read (0x20)
   20 cf.status c!
   cf.busywait
   
   \ stack is still ( addr numsectors )
   \ Read all of the sectors.
   0 ?do
      \ Read one sector 0x200 or 512 bytes
      200 0 do
         \ Read one byte.
         cf.data c@ over c!
         \ Increment the address.
         1+
      loop
      cf.busywait \ Wait for next sector to be available.
   loop
   drop ( the address )
;


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
create sectorbuff 512 allot
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
0A6 constant /fileinfo+buff ( Bytes including linebuffer )

: newfileid ( -- fileid )
   here /fileinfo allot ( Allocate memory )
   dup /fileinfo 0 fill ( Fill with zeroes ) ;

create working_dir /dirinfo allot ;

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
   sectorbuff -rot 1 -rot cf.read ( Read the sector ) ;

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
   swap 0B + c@ 0f and 0f =
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
   \ Load the cluster and sector in the fileid structure.
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

: print-filename ( direntry_addr -- true )
   \ Print a filename and size.
   \ Always return true to continue on to the next file.
   dup >filename cr 0B type  space  >filesize 32bit@ ud.
   true ( Keep going ) ;

: ls ( -- ) ( List the working directory)
   working_dir init_directory
   ['] print-filename working_dir walkdirectory drop ;



: bin ; ( Required by ANS, but we're ignoring it. )

( File access methods )
1 constant r/o  2 constant w/o  3 constant r/w

: findfile ( filestuct 'findfile direntry_addr -- f )
   cr ." checking " dup 0B type cr .s ( DEBUG )
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
   drop 200 ( 512 ) mod ( Get offset into buffer )
   sectorbuff + c@ swap ( Get the character )
   ( c fileid )
   ( Move to next character )
   dup >fileposition d1+! ( increment fileposition )
   ( Check to see if we've wrapped to a new sector )
   dup >fileposition 2@ drop 200 ( 512 ) mod 0= if
      nextsector ( uses fileid ) else drop ( the fileid )
   then
;

: eol?  ( c -- f )  ( Check for end of line )
   dup 0A = swap 0D = or ; ( CR or LF )
( Note: windows CR+LF will show up as two line endings. )
( This should be OK for parsing files. )


\ TODO: read-line has issues!  Fix it up.
: read-line  ( caddr u1 fileid -- u2 flag ior )
   ( Save the start of the line )
   dup dup >fileposition 2@ rot >linestart 2!
   -rot
   ( fileid caddr u1 )
   ( attempt to read u1 characters )
   0 ?do
      cr ." DEBUG FINFO " over finfo
      ( fileid caddr )
      ( Check for EOF )
      over dup eof?           cr ." DEBUG EOF " .s        if
         2drop drop i 0 0 ( no error, just no more data )
         unloop exit then
      ( fileid caddr fileid )
      read-char               cr ." READ-CHAR:  " .s
      2dup swap i + c! ( Save the char )
      ( Check for end of line )
      eol?                    cr ." DEBUG EOL? " .s        if
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
   0 ( IO Result - no error )
;

\ Testing
cf.init fat.init

: lcd.fs s" LCD     FS " ;

lcd.fs r/o open-file drop constant fileid

: catfile  ( fileid -- )  ( Copy file contents to screen )
   >r ( Save fileid )
   begin r@ eof? 0= while r@ read-char emit repeat r> drop ;

: rewind  ( fileid -- )
   ( Move fileposition back to beginning of file )
   0. rot reposition-file drop ;

create buff 80 allot
