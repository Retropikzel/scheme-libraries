String utility library

(*string-replace* str replace replace-with)
(*string-replace* str (replace replace-with) ...)

Replaces the first given string with second on on all occuranes. Or replaces
all given (string string) on all occurances.


Examples:

    (string-replace "foo123bar" "123" "456")
    > "foo456bar"

    (string-replace "foo123bar-no" '("123" "456") '("no" "yes"))
    > "foo456bar-yes"


(*string-format* str vals)

Str should be string containing keys of values surrounded by curly brackets.
Vals should be list of lists containing keys as symbol and value as either
string or number.

Examples:

    (string-format "Hello {name}, I count {n} parenthesis" '((name "Schemer") (n 7)))
    > "Hello Schemer, I count 7 parenthesis"

(*string-capitalize* str)

Capitalizes the first character of given string.

Examples:

    (string-capitalize "hello")
    > "Hello"

(*string-center* str len . char)

Center aligns the str to given len. If char is given it is used, otherwise
space is used.

Examples:

    (string-center "hello" 15)
    > "     hello     "

(*string-ends-with?* str end-str)

Returns #t if given str ends with end-str. #f otherwise.

Examples:

    (string-ends-with? "hello" \#o)
    > #t

    (string-ends-with? "hello" \#e)
    > #f

(*string-expand-tabs* str size)

Expand any #\tab with spaces count size.

Examples:

    (string-expand-tabs "\thello" 2)
    > "  hello"

    (string-expand-tabs "\thello\t" 4)
    > "    hello    "
