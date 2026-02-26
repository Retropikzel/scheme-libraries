LEB128 or Little Endian Base 128 is a variable-length code compression used to store arbitrarily large integers in a small number of bytes.


(**integer->leb128** value)

*value* must be exact integer. Returns the integer as leb128
bytevector.



(**leb128->integer** bv . start-index)

*bv* must be bytevector. Reading of leb128 values start at index 0, unless
*start-index* is given. *start-index* must be exact integer.  Returns exact
integer of leb128 value.



(**leb128->integer-and-length** bv . start-index)

Sams as leb128->integer but returns a pair with integer as car and leb128
bytevector length, as in how many bytes long the leb128 was, as cdr.



(**integer->uleb128** value)

*value* must be exact positive integer or 0. Returns the integer as leb128
bytevector.



(**uleb128->integer** bv)

*bv* must be bytevector. Reading of uleb128 values start at index 0, unless
*start-index* is given. *start-index* must be exact integer.  Returns exact
positive integer or 0 of uleb128 value.



(**uleb128->integer-and-length** bv)

Sams as uleb128->integer but returns a pair with integer as car and uleb128
bytevector length, as in how many bytes long the leb128 was, as cdr.


Resources used:
    https://en.wikipedia.org/wiki/LEB128
        - Mostly turning pseudocode from here to Scheme
    https://github.com/mohanson/leb128
        - Code for leb128 to integer handling as we dont know integer size like in C
