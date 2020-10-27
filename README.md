# roo-compiler

The Fighting Mongooses: Eleanor McMurtry and Jenny Yan.

Our compiler is optimistic: if it encounters a semantic error, it replaces any types with ⊥ and
continues to analyse the rest of the code in order to report as many errors as possible.

## Compiler warnings
Our compiler also extends semantic analysis to warn programmers of possible unintended behaviour in
their code. The warning categories are outlined below. Warnings do not cause compilation to fail.

1. Infinite loop detection: the code
```
    while i < 10 do
        arr[i] <- i;
    od
```
produces a warning
```
test/infinite.roo:7:15: warning: possible infinite loop: the condition does not change between iterations
    while i < 10 do
              ^
```
This does not work in all cases, because it's the halting problem. However, it catches many obvious
mistakes such as forgetting to increment a loop counter.

2. Trivial condition detection: the code
```
    if false then
        write "foo";
    fi
```
produces a warning
```
test/infinite.roo:13:8: warning: `if` condition is always false
    if false then
       ^
```

3. Failure to return: if a procedure is declared with a return type (more on this later) but does
not return a value in all possible paths, a warning is emitted. For example, the code
```
procedure main ()
{
    writeln bad();
}

procedure bad () -> integer
{
    writeln "doing nothing, thanks";

    if false then
        return 0;
    fi
}
```
produces a warning
```
test/return-bad.roo:6:11: warning: control may reach the end of the procedure without returning a value
procedure bad () -> integer
          ^
```

# Extensions to Roo
We have implemented several extension features to the Roo language.

## 1. Function calls and return values
Procedures can now have return values:
```
procedure square(integer val x) -> integer
{
    return x * x;
}
```
This allows procedures to be called in expressions:
```
    writeln 2 * square(3);
    # Output: 18
```

This is implemented à la x86, using the first register to store the return value. For simplicity,
we only support returning primitive types (since they fit in one register easily).

We cheat slightly here. We sometimes need to save registers in complex expressions, and the proper
way to do this is to push a stack frame. However, this would need to be done after arguments are
loaded, and this would clobber the very values we're supposed to be saving. Instead, we use the end
of the register list as a pseudo-stack; this is one of the issues with passing arguments in
registers.

It's worth noting this is done without making `return` a reserved word, using this rather silly
piece of Haskell:
```hs
pKeyword :: String -> Parser ()
pKeyword []  = pure ()

pKeyword [c] = lexeme (char c)
            $> ()

pKeyword str = lexeme (char begin)
            *> mapM_ char middle
            *> lexeme (char end)
            $> ()
    where
        begin  = head          str
        middle = (init . tail) str
        end    = last          str
```

## 2. Tail call optimisation (TCO)
Since procedures can now return values, they can also be recursive in the natural manner:
```
procedure factorial(integer val x) -> integer
{
    if x <= 1 then
        return 1;
    else
        return x * factorial(x - 1);
    fi
}
```
We have implemented tail-call optimisation to better support this use case. A simple example below
demonstrates how TCO avoids stack overflows in deeply recursive calls:
```
procedure main ()
{
    writeln double_tco(100000);
    writeln double_naive(100000);
}

procedure double_naive(integer val x) -> integer
{
    if x > 0 then
        return 2 + double_naive(x - 1);
    else
        return 0;
    fi
}

procedure double_tco(integer val x) -> integer
{
    return double_tco'(x, 0);
}

procedure double_tco'(integer val x, integer val acc) -> integer
{
    if x > 0 then
        return double_tco'(x - 1, acc + 2);
    else
        return acc;
    fi
}
```
Output:
```
❯ ./test.sh test/tco.roo
200000
at instruction 1584017, pc 57: stack overflow
```

We do this by compiling an extra label into the procedure after arguments have been loaded but
before locals have been initialised. If a statement of the form `return this();` is detected, the
compiler emits an unconditional branch instruction instead of a call instruction (so that a new
stack frame is not pushed).
```
proc_double_tco':
# prologue
    push_stack_frame 2
# load args
    store 0, r0
    store 1, r1
proc_double_tco'_loaded:
# if x > 0 then
    load r0, 0
    int_const r1, 0
    cmp_gt_int r2, r0, r1
    branch_on_false r2, label_2
# return double_tco'(x - 1, acc + 2);
# call double_tco'
    load r3, 0
    int_const r4, 1
    sub_int r5, r3, r4
    load r6, 1
    int_const r7, 2
    add_int r8, r6, r7
    store 0, r5
    store 1, r8
    branch_uncond proc_double_tco'_loaded
    move r0, r0
    move r0, r0
    pop_stack_frame 2
    return
    branch_uncond label_3
```

## 3. Higher-order functions (HOF)
Continuing with the functional theme, our compiler also supports higher-order functions.
```
array[10] integer array_t;

procedure main ()
    array_t arr;
    integer i;
{
    while i < 10 do
        arr[i] <- i;
        i <- i + 1;
    od

    call map(square, arr, arr);

    i <- 0;
    while i < 10 do
        writeln arr[i];
        i <- i + 1;
    od
}

procedure square(integer val x) -> integer
{
    return x * x;
}

procedure map (procedure (integer val x) -> integer unary_op, array_t in, array_t out)
    integer i;
    integer x;
{
    while i < 10 do
        out[i] <- unary_op(in[i]);
        i <- i + 1;
    od
}
```
Unfortunately, Oz does not support branching to addresses. This makes the implementation somewhat
tricky. In the end, we opted to use dynamic dispatch via a vtable in a similar manner to C++.
Each procedure is assigned an index, and function pointers hold the value of their procedure's
index. When a function pointer is called, the index is moved to a special-purpose register (r1023)
and the code branches to a vtable section. This section performs a linear scan over the procedure
IDs and calls the appropriate procedure.
```
__vtable:
    int_const r1022, 0
    cmp_eq_int r1021, r1022, r1023
    branch_on_true r1021, __vptr_main
    int_const r1022, 1
    cmp_eq_int r1021, r1022, r1023
    branch_on_true r1021, __vptr_square
    int_const r1022, 2
    cmp_eq_int r1021, r1022, r1023
    branch_on_true r1021, __vptr_map
    branch_uncond __vtable_end
__vptr_main:
    call proc_main
    return
__vptr_square:
    call proc_square
    return
__vptr_map:
    call proc_map
    return
__vtable_end:
    string_const r0, "invalid virtual pointer: "
    call_builtin print_string
    move r0, r1023
    call_builtin print_int
    string_const r0, "\n(exiting)"
    call_builtin print_string
    halt
```
We considered using a binary search strategy instead, but felt the increased number of branches
involved would not lead to a significant benefit in most practical cases.

## 4. Lambda functions
HOF would be very cumbersome without the ability to define inline anonymous functions. To this end,
we introduce the `lambda` keyword:
```
array[10] integer array_t;

procedure main ()
    array_t arr;
    integer i;
{
    while i < 10 do
        arr[i] <- i;
        i <- i + 1;
    od

    call map(lambda (integer val x) -> integer { return x * x; }, arr, arr);
    call for_each(lambda (integer val x) { write x; write " "; }, arr);
    writeln "";
}

procedure for_each (procedure (integer val x) consumer, array_t arr)
    integer i;
{
    while i < 10 do
        call consumer(arr[i]);
        i <- i + 1;
    od
}

procedure map (procedure (integer val x) -> integer unary_op, array_t in, array_t out)
    integer i;
    integer x;
{
    while i < 10 do
        out[i] <- unary_op(in[i]);
        i <- i + 1;
    od
}
```
Again, Oz not supporting branching-to-addresses makes this somewhat awkward. We implement this using
a preprocessing step: before generating symbols, the code is scanned for any lambda declarations,
which are lifted to proper procedures. The above code thus translates to like:
```
array[10] integer array_t;

procedure main ()
    array_t arr;
    integer i;
{
    while i < 10 do
        arr[i] <- i;
        i <- i + 1;
    od

    call map(__lambda0, arr, arr);
    call for_each(__lambda1, arr);
    writeln "";
}

procedure for_each (procedure (integer val x) consumer, array_t arr)
    integer i;
{
    while i < 10 do
        call consumer(arr[i]);
        i <- i + 1;
    od
}

procedure map (procedure (integer val x) -> integer unary_op, array_t in, array_t out)
    integer i;
    integer x;
{
    while i < 10 do
        out[i] <- unary_op(in[i]);
        i <- i + 1;
    od
}

procedure __lambda0(integer val x) -> integer { return x * x; }
procedure __lambda1(integer val x) -> { write x; write " "; }
```
These lambda functions are therefore not closures.

## 5. Modules
Finally, we have implemented rudimentary module support, using the preprocessor approach from C.
Call the below `hof-lib.roo`:
```
array[10] integer array_t;

procedure for_each (procedure (integer val x) consumer, array_t arr)
    integer i;
{
    while i < 10 do
        call consumer(arr[i]);
        i <- i + 1;
    od
}

procedure map (procedure (integer val x) -> integer unary_op, array_t in, array_t out)
    integer i;
    integer x;
{
    while i < 10 do
        out[i] <- unary_op(in[i]);
        i <- i + 1;
    od
}
```
The library can now be loaded in another file:
```
@include "hof-lib.roo"

procedure main ()
    array_t arr;
    integer i;
{
    while i < 10 do
        arr[i] <- i;
        i <- i + 1;
    od

    call map(lambda (integer val x) -> integer { return x * x; }, arr, arr);
    call for_each(lambda (integer val x) { write x; write " "; }, arr);
    writeln "";
}
```
Thus we may implement a Roo standard library. A small sample of such a thing is included in the
`lib/` directory. It goes without saying that the error handling accounts for the possibility of
several files, and reports errors accurately between them.

NOTE: nested includes are not allowed.

Test file for the libraries:
```
@include "../lib/hof.roo"
@include "../lib/math.roo"
@include "../lib/random.roo"

procedure main ()
    array_t arr;
    array_t arr';
    integer gcd;
    integer inverse;
    integer seed;
{
    call naturals(arr);
    call map(lambda (integer val x) -> integer { return x * x; }, arr, arr');
    call filter(lambda (integer val x) -> boolean { return x / 2 * 2 = x; }, arr', arr);
    call for_each(lambda (integer val x) { writeln x; }, arr);

    writeln mod(-3, 2);
    writeln mod(42352, 324);
    writeln modpow(324, 42352, 11);
    writeln mod(1024, 1);
    writeln pow(2, 10);
    call euclid_gcd(37551, 17, gcd, inverse);
    writeln inverse;

    writeln random(seed);
    writeln random(seed);
    writeln random(seed);
    writeln random(seed);
}
```