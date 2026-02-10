Spit and slurp. Read whole file, or output into file.


(*slurp* file-path)

Read whole contents of a file as text.

(*spit* file-path text . append?)

Output the text into a file. If append is #t then text is added to the end.
Otherwise the file is deleted and rewritten.

