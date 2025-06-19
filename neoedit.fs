\ PROGRAMMER  : Sam Colwell
\ FILE        : neoedit.fs
\ DATE        : 2025-06
\ DESCRIPTION : Block editor for neo6502 using built-in editor

\ Name the API locations
$FF00              constant neoControlPort
neoControlPort 1 + constant neoFunction
neoControlPort 2 + constant neoError
neoControlPort 3 + constant neoStatus
neoControlPort 4 + constant neoParameters

\ Variables needed by neoedit
#80 buffer: neoErrorBuffer \ COUNTED STRING - Status info at bottom of screen
#256 buffer: neoLineBuffer  \ COUNTED STRING - current line being edited
\ Note: Counted strings have their length in the first byte.
0 neoErrorBuffer c!
#64 neoLineBuffer c!


\ Bring Tali's assembler into the wordlist search order.
assembler-wordlist >order

\ Use the jump vectors to access these kernel routines.
: neoReadCharacter [ $FFEE jsr ] ; always-native
: neoWaitMessage   [ $FFF4 jsr ] ; always-native
: neoSendMessage   [ $FFF7 jsr ] ; always-native
\ Note: SendMessage expects two bytes after the JSR for the group
\ and function number.  These can be added using
\ [ group# c, function# c, ]
\ just after using this word.

: neoedit ( blk -- )
  \ Save the block number in SCR so neoedit is interchangeable with LIST.
  scr !
  \ Start the editor using the API.
  neoErrorBuffer neoParameters !
  neoSendMessage [ 13 c, 1 c, ] neoWaitMessage
  begin
    \ Get return code
    neoParameters c@   dup while
    case
      1 of ( Initialize )
        \ Give the number of lines.
        \ We're doing 16 lines of 64 chars, with linelength being
        \ enforced by us (the editor doesn't know).
        16 neoParameters ! endof
      2 of ( Get Line )
        \ Get the line number
        neoParameters 1+ @
        \ I believe this is one-based counting.
        \ Make it zero-based.
        1-
        \ Get where line is in Tali buffer.
        #64 *  scr @ block  +
        \ Copy into counted string buffer.
        neoLineBuffer 1+ #64 move
        \ Tell the neo6502 editor where to find it.
        neoLineBuffer neoParameters ! endof
      3 of ( Get Key )
        \ Just call the kernel getkey
        neoSendMessage [ 2 c, 1 c, ] neoWaitMessage endof
      4 of ( Put Line )
        \ Put the counted string on the stack as addr u
        neoLineBuffer count
        \ Get line number
        neoParameters 1+ @ ( addr u  linenum )
        \ I believe this is one-based counting.
        \ Make it zero-based.
        1-                 ( addr u  linenum )
        \ Get where line is in Tali buffer.
        #64 *  scr @ block  +  ( addr u  addr )
        \ TODO: Make the incoming string exactly 64 chars if its
        \ length was changed.
        
        \ Put the counted string there
        swap ( addr addr u ) move   endof
      ( Insert Line and Delete Line not supported and ignored )
    endcase
    \ Go back into the editor.
    neoSendMessage [ 13 c, 2 c, ] neoWaitMessage
repeat ;
        
        
  


