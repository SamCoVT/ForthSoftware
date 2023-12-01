\ PROGRAMMER  : Sam Colwell
\ FILE        : editor.fs
\ DATE        : 2020-05
\ DESCRIPTION : A simple full-screen editor for TaliForth2.
\               Requires an ANSI compatible terminal with
\               the arrow keys in cursor movement mode
\ KEYS :
\ ^X = Save and exit                ^G = Don't save and exit
\ ^A or HOME = beginning of line    ^E or END = end of line
\ INSERT = Toggle INSert/OVeRwrite  ^L = Redraw Screen
\ Arrow keys, backspace, and delete also supported
\ Behavior of delete (backspace/delete) depends on terminal

decimal 
variable ed.row    \ Row (0 based)
variable ed.col    \ Column (0 based)
variable ed.buffer \ Buffer address
variable ed.done
variable ed.inserting

: ed.topline ( -- )  ( Print character count guide at the top )
  ."    +" #64 0 do
    i #10 mod  #9 = if [char] + emit else [char] - emit then
  loop ;

: ed.printline ( n -- ) ( Print the given line 0-based )
  dup 3 +  1 swap  at-xy
  dup 2 u.r space [char] | emit
  #64 *  ed.buffer @ +  #64 type ;

: ed.movecursor ( -- ) ( Move the cursor to current row/col )
  ed.col @ 5 +   ed.row @ 3 +   at-xy ;

: ed.drawinsovr ( -- )  ( Draw INS or OVR on the screen )
  #63 1 at-xy  ed.inserting @ if ." INS" else ." OVR" then
  ed.movecursor ;
  
: ed.drawscreen ( -- )
  page ( Clear the screen )
  1 1 at-xy  ." Screen # " scr @ 5 u.r
  ."    ^X to quit and save, ^G to abort"
  1 2 at-xy ed.topline
  #16 0 do i ed.printline  loop
  ed.drawinsovr ;


: ed.row++ ( -- ) ed.row @ 1+  15  min  ed.row ! ;

: ed.row-- ( -- ) ed.row @ 1-   0  max  ed.row ! ;

: ed.col++ ( -- ) ed.col @ 1+  #63  min  ed.col ! ;

: ed.col-- ( -- ) ed.col @ 1-   0  max  ed.col ! ;

: ed.rowend? ( -- f ) ed.col @  #63 = ;

: ed.nextline ( -- ) 0 ed.col !  ed.row++  ed.movecursor ;

: ed.currentpoint ( -- addr ) ( Address in buffer of cursor )
  ed.buffer @   ed.row @ #64 * +   ed.col @ + ;

: ed.moveeol ( -- ) ( Move to end of line)
  ed.buffer @   ed.row @ #64 * +   #64  -trailing
  #63 min  ed.col !  drop
  ed.movecursor ;

: ed.lineleft ( -- u ) ( Number of chars left in line )
  #64 ed.col @ - ;

: ed.shuffle ( n -- ) ( Shuffle line along n characters )
  ed.lineleft min ( Prevent shuffling off the end )
  ed.currentpoint  2dup +  rot ed.lineleft swap -  move ;

: ed.shuffleback ( -- ) ( Shuffle line back one character )
  ed.currentpoint  dup 1- ed.lineleft move
  bl  ed.currentpoint ed.lineleft + 1-  c! ;

: ed.insertline ( n -- ) ( Insert blank line for given line)
  #15 min  dup #15 <> if ( Only if not on the last line... )
    dup #64 *  ed.buffer @ +           ( Starting address )
    dup #64 +                      ( Address of next line )
    ed.buffer @  #1024 +  over - ( Characters to move )
    move
  then #64 * ed.buffer @ +  #64  bl  fill ; ( Clear line )

: ed.copyeol ( -- ) ( Copy rest of line to pad )
  pad #64 bl fill ( Prepare pad )
  ed.currentpoint pad ed.lineleft move ;

: ed.paste ( -- ) ( Paste pad to current point )
  pad ed.currentpoint ( Source and destination for move )
  ( Determnine how many characters to move )  
  pad #64 -trailing swap drop  ed.lineleft  min
  ( See if there is any line remaining and shuffle it down )  
    dup ed.col @ + ( Column paste will end )
    64 < if dup ed.shuffle then ( shuffle remaining line )
  move ( in memory)
  ed.currentpoint ed.lineleft type ( on screen ) ;

: ed.clreol ( -- ) ( Clear to end of line )
  ed.copyeol ( Save deleted text to pad )
  ed.currentpoint  ed.lineleft  bl   fill ( in memory )
  #64 ed.col @ - spaces ( on screen ) ;

: ed.advance ( -- ) ( Move position one place or to next line )
  ed.rowend? if ed.nextline else ed.col++ then ;

: ed.savekey ( c -- ) ( Save the character into the screen. )
  ed.inserting @ if
    1 ed.shuffle  ed.currentpoint c! ( shuffle and save in mem )
    ed.currentpoint ed.lineleft type
    ed.advance  ed.movecursor ( screen )
    else
      dup emit ( Print it ) ed.currentpoint c! ( Save it )
      ed.advance  ed.col @ 0= if ed.movecursor then 
  then ;

: ed.backspace ( -- ) ( Backspace one character )
  ed.col @ if ed.shuffleback ed.col-- ed.movecursor
              ed.currentpoint ed.lineleft type
              ed.movecursor then ;

: ed.delete ( -- ) ( Delete char at cursor )
  ( this may temporarily end up with 64 in ed.col )
  ed.col @  1+  ed.col !
  ed.backspace ;

: ed.processkey ( c -- )
  case #01 ( CTRL-A ) of 0 ed.col !  ed.movecursor endof
       #05 ( CTRL-E ) of ed.moveeol  ed.movecursor endof
       #24 ( CTRL-X ) of update   flush   1 ed.done ! endof
       #07 ( CTRL-G ) of empty-buffers
                         scr @ block drop ( reload )
                         1 ed.done ! endof
       #11 ( CTRL-K ) of ed.clreol ed.movecursor endof
       #12 ( CTRL-L ) of ed.drawscreen endof
       #25 ( CTRL-Y ) of ed.paste ed.movecursor endof
       #09 ( CTRL-I ) of 16  ed.row @   dup ed.insertline
                         do i ed.printline loop
                         ed.movecursor endof
       #27 ( ESC ) of
         key  dup  [char] [ =  [char] O =  or  if
           key case [char] A ( Up Arrow )    of ed.row-- endof
                    [char] B ( Down Arrow )  of ed.row++ endof
                    [char] C ( Right Arrow ) of ed.col++ endof
                    [char] D ( Left Arrow )  of ed.col-- endof
                    [char] H ( Home )        of 0 ed.col ! endof
                    [char] F ( End  )        of ed.moveeol endof
                    [char] 2 ( Insert )      of
                      ed.inserting @ invert  ed.inserting !
                      ed.drawinsovr
                      key drop ( eat the ~ ) endof
                    [char] 3 ( Delete)       of
                      ed.delete      
                      key drop ( eat the ~ ) endof
               endcase  ed.movecursor
           then
       endof
       #13 ( CR )       of ( ed.clreol ) ed.nextline endof
       #08 ( BS )       of ed.backspace endof
       #127 ( also BS ) of ed.backspace endof
       \ Any other character
       dup ed.savekey
  endcase ;

: edit ( u -- ) ( Edit the given screen )
  dup scr !  block ed.buffer !  0 ed.done !  TRUE ed.inserting !
  0 ed.row !  0 ed.col !  ed.drawscreen
  begin key ed.processkey   ed.done @ until   #1 #19 at-xy ;
  

\ Testing:
: testkey ( -- ) ( Print ASCII values - q to quit )
  begin key  dup .  [char] q = until ; 
\ 4 block-ramdrive-init
\ 1 edit

\ For systems without at-xy
\ : ns. ( u -- ) ( Print # w/ no space ) base @ swap 0 <# #s #> type  base ! ;
\ : at-xy ( x y -- ) ( Move cursor )
\   #27 emit  [char] [ emit  swap ns.  [char] ; emit  ns.  [char] H emit ;
