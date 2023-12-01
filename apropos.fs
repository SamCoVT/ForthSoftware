\ PROGRAMMER  : Sam Colwell
\ FILE        : apropos.fs
\ DATE        : 2023-12
\ DESCRIPTION : Print all words in the current wordlist that contain the given string.
\               This is similar to the emacs apropos command.
\ USAGE       : apropos stuff ( to find all words with "stuff" in their names )
\               Note that redefined words will be listed multiple times as this
\               will find the old versions as well.

: apropos ( "<spaces>name_fragment" -- )
  ( Print word names that contain name_fragment )
  bl parse  2>r \ Get name_fragment and save on return stack.
  cr            \ Start on a blank line.
  latestnt      \ Most recent word in dictionary
  begin dup 0<> while \ Loop through the dictionary entries.
    dup name>string
    \ See if this name has the name_fragment in it.
    2dup  2r@  search  nip nip if
      \ Print the word
      type space cr
    else
      \ Don't print the word, but remove it from the stack.
      2drop
    then
    \ Follow the dictionary linked list back.
    2 + @ repeat
  \ Clean up the stack and return stack.
  drop  2r> 2drop ;
  


\ Original version:

\ 2variable apropos_string
\ : apropos ( addr u -- ) ( Print word names that contain given string )
\   apropos_string 2! \ Save the string info
\   latestnt
\   begin dup 0<> while
\     dup name>string
\     \ See if this name has the string in it.
\     2dup apropos_string 2@ search nip nip if
\       \ Print the word
\       type space cr
\     else
\       \ Don't print the word, but remove it from the stack.
\       2drop
\     then
\     \ Follow the dictionary linked list.
\     2 + @ repeat
\   drop ;
