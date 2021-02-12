# SQL-parser
Right now the "pretty printer" implements a rather small subset of sql. It includes:
- basic select with expressions and nested expressions
- basic select options (ALL, DISTINCT etc)
- select ... from ... ;

First time writing anything in c so... Please don't be too disgusted :D Pretty different coming from a python world. But really fun. Memory leaks abound. 

The main recursion for reconstructing the string from the RPN is extremely inefficient memory wise right now. It causes an additional stack frame to be made for almost every production rule. Due to the representation being in RPN, each non-terminal in a production has to be constructed last to first. After all the strings for non-terminals have been constructed, I construct the output string by concatenating them in the correct order. This could be optimized pretty easily by constructing the whole string in reverse. I just didn't have time to do so :(

To build and test:
```
git clone https://github.com/luk-pio/SQL-parser.git
cd SQL-parser
make sql test
```
