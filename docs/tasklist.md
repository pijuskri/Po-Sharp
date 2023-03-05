## LLVM migration

* lambdas

## Bugs
* ~~functions have limited checking before llvm catches it (especially multiple constructors)~~
* remove l prefix from register names

## To do


* make template functions explicit (diff syntax, otherwise conflict in template interfaces)
* add copy command to arrays(and maybe structs)
* rework file imports with explicit export command
* add more object intrinsics(equal)
* file name in error messages
* line numbers in compiler
* static variables
* add prinf
* Allow selecting main function/file

* infer self in object functions
* add method to get all interface arguments/names
* function reflection(do this by declaring labels in file with strings and var names)
* parser error reporting
* nested arrays
* add runtime index checking for arrays
* optimise string to char array conversion

* ~~Prevent array deferencing (point to a pointer that points to array)~~
* ~~booleans~~
* ~~change functions names to consider their object~~
* ~~make self auto-pass~~
* ~~bug when comment at top of file~~
* ~~allow list of . calls~~