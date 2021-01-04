\ Multiply unsigned double by unsigned, resulting in unsigned double.
: du* ( du u -- du ) dup >r um* drop swap r> um* rot + ;
