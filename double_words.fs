( SamCo ANS double words including ext. words 1/2 2020-01 )
( Tali 2 has the following words already: 2CONSTANT       )
( 2LITERAL 2VARIABLE D+ D- D. D.R D>S DABS DNEGATE        )
assembler-wordlist >order  ( Bring in assembler )
: D0=  ( xd -- flag )  ( flag is true iff xd is zero )
  or 0= ; allow-native
: D2*  ( xd1 -- xd2 )  ( left shift one bit )
  [ 2 asl.zx 3 rol.zx 0 rol.zx 1 rol.zx ] ; allow-native
: D2/  ( xd1 -- xd2 )  ( right shift one bit )
  [ 1 lsr.zx 0 ror.zx 3 ror.zx 2 ror.zx ] ; allow-native
: D<   ( d1 d2 -- flag )  ( flag is true iff d1 < d2 )
  rot 2dup = if 2drop u< else 2swap 2drop > then ;
: D=   ( xd1 xd2 -- flag )  ( flag is true iff d1 = d2 )
  rot = -rot = and ; allow-native


( SamCo ANS double words including ext. words 2/2 2020-01 )
( M*/ from All About FORTH, MVP-Forth, public domain )
( Note: This version allows n2 to be negative    )
: M*/  ( d1 n1 n2 -- d2 ) ( modified slightly for Tali2 )
DDUP XOR SWAP ABS >R SWAP ABS >R OVER XOR ROT ROT DABS
SWAP R@ UM* ROT R> UM* ROT 0 D+ R@ UM/MOD ROT ROT R> UM/MOD
SWAP DROP SWAP ROT 0< if dnegate then ;
: M+  ( d1|ud1 n -- d2|ud2 )  ( Add single to double )
  0 ( Make the single a double)  d+ ; allow-native
( TODO: Extension words 2ROT, 2VALUE )
: DU<  ( ud1 ud2 -- flag )  ( flag is true iff ud1 < ud2 )
  rot 2dup <> if swap 2swap then 2drop u< ;

( Still to be written: DMAX DMIN D0< 2ROT )



\ misc useful words (not part of the ANS standard)
\ Multiply unsigned double by unsigned, resulting in unsigned double.
\ Perhaps this should be named dm*?
: du* ( du u -- du ) dup >r um* drop swap r> um* rot + ;
