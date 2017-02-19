EECS 345: Programming Language Concepts
---------------------------------------

Programming Project, Part 1
---------------------------

### Due Monday, February 20 by 11:59 pm

*For this and all programming project’s, you are welcome to work in groups of up to three. The names of all group members should appear at the top of the file, and every member should submit the project on blackboard. All team members are responsible for understanding the code submitted in their name.*

In this homework, you are to create an interpreter for a very simple Java/C-ish language. The language has variables, assignment statements, mathematical expressions, comparison operators, boolean operators, if statements, while statements, and return statements.

An example program is as follows:

    var x;
    x = 10;
    var y = 3 * x + 5;
    while (y % x != 3)
      y = y + 1;
    if (x > y)
      return x;
    else if (x * x > y)
      return x * x;
    else if (x * (x + x) > y)
      return x * (x + x);
    else
      return y - 1;

</p>
Note that braces, `{` and `}`, are not implemented.

The following mathematical operations are implemented : `+, -, *, /, %` (including the unary `-`), the following comparison operators are implemented: `==, !=, <, >, <=. >=`, and the following boolean operators: `&&, ||, !`. Variables may store values of type `int` as well as `true` and `false`. You do not have to detect an error if a program uses a type incorrectly, but it is not hard to add the error check.) Note that you do not have to implement short-circuit evaluation of `&&` or `||`.

**For those seeking an extra challenge:** The parser supports nested assignment statements as well as assignments inside expressions. Try writing your interpreter so that assignment operators return a value as well as initialize a variable:

    var x;
    var y;
    x = y = 10;
    if ((x = x + 1) > y)
      return x;
    else
      return y;

</p>
**General guidelines**

You are to write your interpreter in Scheme using the functional programming style. For full marks, you should not use variables, only functions and parameters.

Your program should clearly distinguish, by naming convention and code organization, functions that are doing the `M_state` operations from ones doing the `M_value` and `M_boolean` operations. You do not have to call them `M_`, but your naming convention should be consistent.

A parser is provided for you called `simpleParser.scm`. You will also have to get the file `lex.scm`. You can use the parser in your program by including the line `(load "simpleParser.scm")` at the top of your homework file. The command assumes `simpleParser.scm` is in the same directory as your homew
