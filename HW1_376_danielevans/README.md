Daniel Evans
September 27, 2016
CS376 HW1

The project consists of writing a parser and interpreter for the WAE language, and extend the language with a single rule for binary arithmetic operators (as opposed to a new rule for each binary operator) and by implementing a multi-armed with, which is used to assign multiple variables in a single with statement. 

The parse function takes in a concrete syntax and outputs the abstract grammer of the WAE language. This abstract syntax can be passed to the interp function, which will in turn output an evaluation of the syntax in the form of a number.

Furthermore, the usefulness of having a single rule for binary arithmetic operators is that it makes the language easier to extend. Take the example of bitwise operators; instead of writing separate rules into the WAE datatype and writing more code checking for bitwise operators in both the parser and interpreter functions, all the programmer has to do is implement a function that will output the result of performing the bitwise operators and map that function to a symbol. This symbol will be used by a lookup function during the interpretation process to find the correct procedure for bitwise operations. This reduces both the potential for coding errors during language extension and the amount of source code in project. 

Likewise, the multi-armed with allows the programmer to define multiple bindings within a single with statement rather than having to define multiple with statements, allowing him or her to write more readable, succint code.

Finally, all functions/datatype variants (with, binary arithmetic operators, helper functions) work as expected. That is, they output the correct result for the given input, and there are no problems with the code.  
