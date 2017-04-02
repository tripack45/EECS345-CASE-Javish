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

A parser is provided for you called `simpleParser.scm`. You will also have to get the file `lex.scm`. You can use the parser in your program by including the line `(load "simpleParser.scm")` at the top of your homework file. The command assumes `simpleParser.scm` is in the same directory as your home

## Interpreter Project, Part 2

### Due Tuesday, March 21

*For this and all Interpreter Project's, you are welcome to work in groups, but each person is expected to submit and be responsible for their own interpreter.*

In this homework, you will expand on the interpreter of part 1 adding code blocks as well as "goto" type constructs: break, continue, (true) return, and throw. and continue and blocks. We still assume all variables store either an integer or a boolean value. _For those wanting an extra challenge_: you are to again assume that expressions can have side effects. Specifically, you should assume that any expression can include an assignment operator that returns a value.

Please note: a portion of your grade in this project will be correcting the errors you had in Part 1.

The parser you used in part 1 supports all of the language features used in this assignment. Here are the new language constructs you need to implement:
```
break;              =>   (break)
continue;           =>   (continue)
throw e;            =>   (throw e)
```

```
if (i < j) {       =>   (if (< i j) (begin (= i (+ i 1)) (= j (+ j 1))))
  i = i + 1;
  j = j - 1;
}
```

```
try {               =>   (try _body_ (catch (e) _body_) (finally _body_))
  _body_
}
catch (e) {
  _body_
}
finally {
  _body_
}
```

Note that either the finally or the catch block may be empty:

```c++
try {                    =>  (try _body_ (catch (e) _body_) ())
  _body_
}
catch (e) {
  body
}
```

Please note:

*   As with C and Java, a block of code can appear anywhere and not only as the body of an if statement or a loop.
*   As with C and Java, the break and continue apply to the immediate loop they are inside. There are no labels in our interpreter, and so there will be no breaking out of multiple loops with one break statement.
*   As there is no type checking in our language, only one catch statement per try block is allowed.

#### Style Guidelines

You do not have to stick to strict functional programming style, but you should avoid global variables and heavy use of <tt>let</tt> because they will make your life harder. You also should not use <tt>set!</tt> (except for the recommended state change below).

As with the last project, your program should clearly distinguish, by naming convention and code organization, functions that are doing the <tt>M_state</tt> operations from ones doing the <tt>M_value</tt> and <tt>M_boolean</tt> operations.

Also as before, the launching point of your interpreter should be a function called <tt>interpret</tt> that takes a filename, calls <tt>parser</tt> with the filename, evaluates the parse tree returned by <tt>parser</tt>, and returns the proper value. You are to maintain a state for the variables and return an error message if the user attempts to use a variable before it is initialized.

#### Implementing the "Goto" constructs

You need to use continuations to properly implement return, break, continue, and throw. For each, you have two options. You can make your interpreter tail-recursive with continuation passing style (note that only the M_state functions must be tail recursive) or you can use call/cc. Both techniques are equally challenging. You are also welcome to use cps for some of the constructs and call/cc for others.

#### The Program State

To implement blocks, you need to make the following **required** change to the state/environment. In addition, because this interpreter does not require a lot of new features from the previous one, there is a **recommended** change to the state that may help reduce the work required when we get to Part 3 of the interpreter.

**The required change**: Your state must now be a list of _layers_. Each layer will contain a list of variables and bindings similar to the basic state of part 1\. The initial state consist of a single layer. Each time a new block is entered, you must "cons" a new layer to the front of your state (but use abstraction and give the operation a better name than "cons"). Each time a variable is declared, that variable's binding goes into the top layer. Each time a variable is accessed (either to lookup its binding or to change it), the search must start in the top layer and work down. When a block is exited, the layer must be popped off of the state, deleting any variables that were declared inside the block.

**A reminder about a note from part 1**: Your state needs to store binding pairs, but the exact implementation is up to you. I recommend either a list of binding pairs (for example: <tt>((x 5) (y 12) ...)</tt> ), or two lists, one with the variables and one with the values (for example: <tt>((x y ...) (5 12 ...))</tt>). The first option will be simpler to program, but the second will be more easily adapted supporting objects at the end of the course.

**The recommended change**: In Part 3 of the interpreter, you will need to implement function/method calls and global variables. Thus, even if you are not doing the extra coding challenge, you will need to handle functions that produce side effects. If you would like a simpler way to deal with side effects, I recommend the following break from strict functional style coding. Instead of binding each variable to its value, we will bind the variable to a _box_ that contains its value. You can think of a box as a pointer to a memory location, and thus the values stored in the environment will be pointers to the actual data (similar to how Java implements non-primitive types). Using boxes, you will not need separate <tt>M_value</tt> and <tt>M_state</tt> functions for handling function calls. Instead, the function/method call <tt>M_value</tt> mapping will be able to change the values of global variables. The Scheme commands are:  
<tt>(box v)</tt>: places _v_ into a _box_ and returns the box  
<tt>(unbox b)</tt>: returns the value stored in box _b_  
<tt>(set-box! b v)</tt>: changes the value stored in box _b_ to value _v_.  
Note that the <tt>set-box!</tt> command does not return a value. You should embed it in a <tt>begin</tt> function. Scheme <tt>begin</tt> takes one or more expressions and returns the value of the last expression. For example, <tt>(begin (set-box! b v) #t)</tt> will return <tt>#t</tt>.



EECS 345: Programming Language Concepts
---------------------------------------


Interpreter Project, Part 3
---------------------------

### Due Wednesday, April 12

In this homework, you will expand on the interpreter of part 2 adding
function definitions. We still assume all variables store integers and
boolean. Likewise, all functions will only return integers and boolean.

While normal C does not allow nested functions, the gcc compiler *does*
allow nested functions as an extension to C, so let's implement them!

*For those seeking a small extra challenge:* try implementing both the
*call-by-reference* and the *call-by-value* parameter passing styles.

An example program that computes the greatest common divisor of two
numbers is as follows:

    var x = 14;
    var y = 3 * x - 7;
    function gcd(a,b) {
      if (a < b) {
        var temp = a;
        a = b;
        b = temp;
      }
      var r = a % b;
      while (r != 0) {
        a = b;
        b = r;
        r = a % b;
      }
      return b;
    }
    function main () {
      return gcd(x,y);
    }

Here is another example program that uses recursion:

    function factorial (x) {
      if (x == 0)
        return 1;
      else
        return x * factorial(x - 1);
    }
    
    function main () {
      return factorial(6);
    }

Note that only assignment statements are allowed outside of functions.
Functions do not have to return a value. The parser will have the
following additional constructs:

    function a(x, y) {          =>   (function a (x y) ((return (+ x y)))
      return x + y;
    }
    
    function main () {          =>   (function main () ((var x 10) (var y 15) (return (funcall gcd x y))))
      var x = 10;
      var y = 15;
      return gcd(x, y);
    }

The final value returned by your interpreter should be whatever is
returned by `main`.

Nested functions can appear anywhere in the body of a function. Any name
in scope in a function body will be in scope in a function defined
inside that body.

    function main() {
      var result;
      var base;
    
      function getpow(a) {
         var x;
    
         function setanswer(n) {
            result = n;
         }
    
         function recurse(m) {
           if (m > 0) {
             x = x * base;
             recurse(m-1);
           }
           else
             setanswer(x);
         }
    
         x = 1;
         recurse(a);
      }
      base = 2;
      getpow(6);
      return result;
    }

We will use a similar style as C++ for call-by-reference:

    function swap(&x, &y) {     =>  (function swap (& x & y) ((var temp x) (= x y) (= y temp)))
      var temp = x;
      x = y;
      y = temp;
    }

Function calls may appear on the right hand side of global variable
declaration/initialization statements, but the function (and any
functions that function calls) must be defined before the variable
declaration. Otherwise, functions that are used inside other functions
do not need to be defined before they are used.

It is an error to use call-by-reference on anything other than a
variable. For example, if the program contains `swap(x, x + 10)` with
the above definition of `swap`, you should give an error because
`x + 10` is not a variable.

You do not have to stick to strict functional programming style, but you
should avoid global variables because they will make your life harder. A
new parser is provided for you, `functionParser.scm`, that will parse
code containing functions/methods as in the above examples. To use the
parser, type the code into a file, and call `(parser "filename")` as
before. To call the parsers from your interpreter code, place the
command `(load "parsename")` in the Scheme file.

#### What your code should do

You should write a function called `interpret` that takes a filename,
calls `parser` with the filename, evaluates the parse tree returned by
`parser`, and returns the proper value returned by `main`. You are to
maintain an environment/state for the variables and return an error
message if the program attempts to use a variable before it is declared,
attempts to use a variable before it is initialized, or attempts to use
a function that has not been defined.

#### Some hints

**Terminology** In this interpreter, we will be talking about
*environments* instead of *states*. The state consists of all the active
bindings of your program. The environment is all the active bindings
that are in scope.

1\. Note that the base layer of your state will now be the global
variables and functions. You should create an outer "layer" of your
interpreter that just does M\_state functions for variable declarations
and function definitions. The declarations and assignments should be
similar to what you did in your part 2 interpreter. The functon
definitions will need to bind the function closure to the function name
where the closure consists of the formal parameter list, the function
body, and a function that creates the function environment from the
current environment.

2\. Once the "outer" layer of your interpreter completes, your
interpreter should then look up the `main` function in the state and
call that function. (See the next step for how to call a function.

3\. You need to create a M\_value function to call a function. This
function should do the following: (a) create a function environment
using the closure function on the current environment, (b) evaluate each
actual parameter in the current environment and bind it to the formal
parameter in the function environment, (c) interpret the body of the
function with the function environment. Note that interpreting the body
of the function should be, with one change, *exactly* what you submitted
for *Interpreter, Part 2*. Also note that if you are using boxes, you
should not have to do anything special to deal with global variable side
effects. If you are not using boxes, you will need to get the final
environment from evaluating the function body and copy back the new
values of the global variables to the current environment/state.

4\. Change the M\_state and M\_value functions for statements and
expressions, respectively, to expect function calls.

5\. Test the interpeter on functions without global variables, and then
test your functions using global variables. One tricky part with the
functions is that, unlike the other language constructs we have created,
function calls can be a statement (where the return value is ignored),
and an expression (where the return value is used). You need to make
sure both ways of calling a function works.

6\. Since exceptions can happen anywhere that a function call can occur,
you may discover more places that need the throw continuation. If you
used call/cc for throw, then you should not have to modify anything else
in your interpreter from part 2. If you used tail recursion for throw,
you will need to make the M\_value functions tail recursive for throw to
work correctly.
