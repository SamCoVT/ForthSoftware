\ PROGRAMMER  : Sam Colwell
\ FILE        : yloop.fs
\ DATE        : 2021-04
\ DESCRIPTION : For small loops of 256 or less, these words will use
\ the Y register in cowntdown mode only (eg load with 8-bit starting
\ count and it will always count down to zero) which is generally useful
\ for code that needs to run a number of times, but doesn't need to
\ control the direction of the counting (in exchange for speed).

\ Add the assembler.
assembler-wordlist >order

: ydo  ( C: -- addr) ( R: n --)  ( Start loop that will run n times )
  0 postpone ldy.x          \ Load Y with the starting value.
  postpone inx postpone inx \ Remove value from stack.
  here          \ Save location to loop back to (leave on stack) 
  postpone phy  \ Save current index to return stack.
; immediate compile-only

: yloop ( C: addr -- ) ( R: -- ) ( Loop back up to start if y nonzero)
  postpone ply    \ Get current index from return stack.              
  postpone dey    \ Count this iteration.                             
  3 postpone beq  \ If we reached zero, continue on (branch over jump)
  postpone jmp    \ otherwise jump to the top of the loop.
  \ The jump should pull it's address from the stack
  \ It was placed there by the ydo word.
; immediate compile-only

: yi
  \ The current index is on the return stack.
  postpone pla \ Get the index into A and put it on the Forth stack.
  postpone pha
  postpone push-a
; immediate compile-only

: yj
  \ The index from the outer loop is the SECOND byte on the return stack.
  postpone ply \ Pull the one we don't want into Y 
  postpone pla \ Pull the one we do want into A
  postpone pha \ Put both of them back (in the right order)
  postpone phy
  postpone push-a \ Put the J index on the Forth stack.
; immediate compile-only



\ Try it out.
: testing
  5 ydo
     3 ydo
        cr ." yi=" yi .  ." yj=" yj .
     yloop
  yloop
;


\ Results:
testing 
yi=3 yj=5 
yi=2 yj=5 
yi=1 yj=5 
yi=3 yj=4 
yi=2 yj=4 
yi=1 yj=4 
yi=3 yj=3 
yi=2 yj=3 
yi=1 yj=3 
yi=3 yj=2 
yi=2 yj=2 
yi=1 yj=2 
yi=3 yj=1 
yi=2 yj=1 
yi=1 yj=1  ok


\ Cycle-Test RESULTS:
decimal
: testingy   255 ydo  yloop ;
: testingdo  255 0 do  loop ;
: testingyi  255 ydo  yi drop  yloop ;
: testingdoi 255 0 do  i drop  loop ;
: testingyy    255 ydo   255 ydo  yloop  yloop ;
: testingdodo  255 0 do  255 0 do  loop  loop ;
: testingyyij    255 ydo   255 ydo yi yj 2drop  yloop  yloop ;
: testingdodoij  255 0 do  255 0 do  i j 2drop  loop  loop ;

' testingy           cycle_test   CYCLES: 3660  ok
' testingdo          cycle_test   CYCLES: 16775  ok

' testingyi          cycle_test   CYCLES: 13605  ok
' testingdoi         cycle_test   CYCLES: 32075  ok

' testingyy          cycle_test   CYCLES: 933900  ok
' testingdodo        cycle_test   CYCLES: 4291340  ok

' testingyyij        cycle_test   CYCLES: 5420625  ok
' testingdodoij      cycle_test   CYCLES: 11053940  ok
