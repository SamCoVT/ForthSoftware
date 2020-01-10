\ PROGRAMMER  : Sam Colwell
\ FILE        : fat32_tali.fs
\ DATE        : 2020-01
\ DESCRIPTION : My initial attempts at Compact Flash
\ and FAT32 support for my 65C02 Single Board Computer
\ using Tali Forth 2.


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

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

\ fat variables
create sectorbuff 512 allot
2variable sector# \ The disk sector currently in the buffer

2variable partition_lba_begin
2variable fat_begin_lba
2variable cluster_begin_lba
variable sectors_per_cluster
2variable root_dir_first_cluster

\ file access variables
create filename 0B allot  \ Name of current file
2variable current_cluster \ The cluster we are on right now
2variable filesize        \ The size of the file
2variable fileoffset      \ The current offset in the file

\ Tali needs some extra double words.
: d=  ( d1 d2 -- f )
   rot = -rot = and ;
: d0=   ( d -- f ) 0= swap 0= and ;
: d0<>  ( d -- f ) or ;
: d2*   ( d -- f ) 2dup d+ ;
: d<    ( d -- f ) rot 2dup = if ( use LSBs ) 2drop <
   else ( use MSBs ) 2swap 2drop > then ;
\ Tali's 2@ doesn't do things in the order we need.
: 32bit@ dup @ swap 2 + @ ;

\ A word for making offsets
: offset  ( u "name" -- ) ( addr -- addr )  create ,  does> @ + ;
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

\ Primatives for initfat.

\ Given the address of a partition entry in the partition table
\ determine if it's fat32 or not.
: fat32part?  ( addr_partentry -- f )
   >parttype c@   dup 0B = swap 0C =   or ; allow-native

\ Read sector (a double) into sectorbuffer.
: sector>buffer  ( sector#.d -- )
   2dup sector# 2@ d= if 2drop exit ( already have this sector ) then
   2dup sector# 2!
   sectorbuff -rot 1 -rot cf.read ;

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
   sectorbuff >rootclust 32bit@   root_dir_first_cluster 2!
;

hex
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

: cluster>sector  ( cluster# -- sector# )  \ Get starting sector for cluster
   2. d-
   \    sectors_per_cluster *
   \ We don't have 32-bit *, so fake it with left shifts.
   \ This works because sectors_per_cluster is always
   \ a power of 2.
   sectors_per_cluster @ log2 dlshift
   cluster_begin_lba 2@ d+ ;
( todo: remove offset by 2 )

: cluster>fatsector  ( cluster# -- sector# ) \ Get sector for fat.
   \ There are 128 FAT pointers in a sector.
   \  80 ( 128 ) /
   \ We don't have double division, so use right shifts.
   7 drshift
   
   \ Start at the fat LBA sector.
   fat_begin_lba 2@ d+
;

2variable current_sector
2variable current_cluster
: next_cluster#  ( -- cluster# )  \ Determine next cluster
   \ Load the correct fat sector into the buffer.
   current_cluster 2@   cluster>fatsector sector>buffer
   \ index it and get the new cluster number.
   current_cluster 2@
   drop ( we don't need the MSB )
   80 mod \ Index into sector
   4 * ( 32-bit clusters )
   sectorbuff + \ Locate the next pointer
   32bit@ \ Read the cluster pointer.
;

: next_sector  ( -- )  ( Move sectorbuffer to next sector )
   \ See if we can just move to the next sector.
   current_sector 2@ 1. d+ cluster_begin_lba 2@ d-
   drop ( we don't need the upper bits )
   sectors_per_cluster @ mod if 
      current_sector 2@ 1. d+   2dup   current_sector 2!
      sector>buffer
   else
      \ We need to move to the next cluster.
      next_cluster#
      2dup current_cluster 2! \ Save this as the current cluster.
      cluster>sector \ Turn it into a sector.
      \ Save this as the current sector and load it.
      2dup   current_sector 2!   sector>buffer
   then
;


2variable directory_index
\ Primitives for handling directories
: directory_offset  ( -- offset_in_sectorbuffer )
   directory_index 2@
   drop ( MSB is not needed )
   10 ( 16 records/sector ) mod
   20 ( 32-bytes/record) * ;

: direntry_invalid?  ( addr_direntry -- f )
   dup c@ E5 = \ See if it begins with E5
   \ Offset to the ATTRIB and see if it has all 4 lower bits
   \ set.
   swap 0B + c@ 0f and 0f =
   \ If either of these things are true, it's an invalid entry.
   or
;

: dir_next
   \ Move along one entry
   directory_index 2@ 1. d+   2dup   directory_index 2!
   \ See if a new sector is needed.
   drop ( don't need MSB for this )
   10 ( 16 entries/sector ) mod 0= if next_sector then
;

: move_to_cluster  ( cluster#.d -- )
   \ Load the starting sector
   2dup   current_cluster 2!   cluster>sector
   2dup   current_sector 2!   sector>buffer ;

: init_directory  ( clusternum.d -- )
   \ Set all of the variables for walking a directory.
   move_to_cluster
   \ Start the the beginning of the directory.
   0. directory_index 2! ;  

: dirend?  ( addr -- f )  c@ 0= ;

: walkdirectory  ( xt -- addr | 0)
   \ run routine (given by xt) on every starting address of a
   \ directory entry.  The routine should
   \ be ( addr -- continueflag ) where continueflag is 1 if
   \ directory processing should continue. The return value of
   \ walkdirectory will either be the address of the directory
   \ entry it stopped on (in the sector buffer) or 0 if it reached
   \ the end.
   
   
   
   \    cr ." DEBUG Before begin " .s
   begin
      \    cr ." DEBUG: directory_index=" directory_index ?
      \ Get the offset of the directory entry into the
      \ sectorbuffer.
      directory_offset
      \ Turn it into an address in the sectorbuffer.
      sectorbuff +
      \ See if we have reached the end of the directory.
      dup dirend? if
         drop ( direntry ) drop ( xt ) 0 ( false ) exit then
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
         dir_next
   repeat
   \ If we made it here, we were told to stop.
   drop ( the xt )
   \ Leave the address of the directory entry on the stack.
   sectorbuff directory_offset +
;

: print-filename ( direntry_addr -- true )
   \ Print a filename and always return true to continue on to
   \ the next file.
   dup >filename cr 0B type   >filesize 32bit@ ud.   true
;

: ls ( -- ) ( List the root directory)
   root_dir_first_cluster 2@   init_directory
   ['] print-filename walkdirectory drop ;

: findscreens.fs ( direntry_addr -- f ) 0B s" SCREENS FS " compare ;

: examplecat
   root_dir_first_cluster 2@   init_directory
   ['] findscreens.fs walkdirectory
   \ Dir entry on stack.  Get size and first cluster.
   dup >filesize 32bit@ ( size ) 
   rot \ Put behind dir entry
   dup >clustLOW @ swap ( first cluster LSB )
   >clustHI @     ( first cluster MSB )
   \ stack is now: size cluster ( both doubles )
   move_to_cluster 
   begin
      \ remaining size is on the stack.
      2dup d0<>
   while
         2dup ( size remaining )
         \ See if there is less than on sector left.
         \ We don't have double comparison words, so just
         \ check each cell.
         0= ( MSB ) swap 200 < ( LSB ) and
         if \ less than 1 sector left?
            \ Type what's left
            drop ( MSB not needed ) sectorbuff swap type
            0. \ Put a 0 on the stack at the count to end things.
         else
            sectorbuff 200 type \ size to type out.
            200. d- \ reduce bytes left
            next_sector \ Move to next sector
         then
   repeat
   2drop
;

cf.init fat.init


\ Words to allow a file to be used as a block source.
create blockfilename 0B allot ( filename without dot )
( start with block.blk )
s" BLOCK   BLK" blockfilename swap move

: findblockfile 0B blockfilename 0B compare ;
: block>sector  ( block# -- sector# )
   root_dir_first_cluster 2@   init_directory
   ['] findblockfile walkdirectory
   dup 0= abort" Can't find block file"
   swap >r ( Save the block number )
   \ Dir entry on stack.  Get size and first cluster.
   dup >clustLOW @ swap ( first cluster LSB )
   dup >clustHI @ swap  ( first cluster MSB )
   >filesize 32bit@ ( size ) 
   \ stack is now: cluster# size ( both doubles )
   ( Check the size )
   r> dup >r ( bring back copy of block number )
   1+ ( end of this block is beginning of next block )
   0 ( make double ) 0A dlshift ( multiply by 1024 )
   d< abort" Block file not large enough"
   
   \ TODO: Adjust variables without loading buffer.
   \ This code loads every sector on the way to the
   \ one it wants.
   move_to_cluster
   r> ( Bring back the block number. )
   0 ?do next_sector next_sector ( blocks are two sectors )
   loop
   current_sector 2@
;

: blockfilereader  ( addr block# -- )
   2 swap ( 2 sectors in a block )
   block>sector
   ( stack is now addr numsectors sector# )
   cf.read ;

: blockfilewriter  ( addr block# -- )
   ( not currently implemented )
   s" TODO: Write block " . drop ;

: cf+i2c_reader  ( addr block# -- )
   ( blocks 0-EFFF go to compact flash, F000-FFFF to I2C )
   dup f000 u<
   if blockfilereader else f000 - eeprom-blockread then ;

: cf+i2c_writer  ( addr block# -- )
   ( blocks 0-EFFF go to compact flash, F000-FFFF to I2C )
   dup f000 u<
   if blockfilewriter else f000 - eeprom-blockwrite then ;

' cf+i2c_reader BLOCK-READ-VECTOR !
' cf+i2c_writer BLOCK-WRITE-VECTOR !
   
   