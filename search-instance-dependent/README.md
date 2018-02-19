
This directory contains the source code which is compiled **after** the problem instance is loaded.

Using the runtime information such as the state encoding length or the number of
operators, this allows for further optimization, including the more compact
state information, pruning, inlining and constant folding to achieve the
maximum performance.


# option 1 XXX

Builder function holds the definition of the function as a literal, or in a special variable.

Calling the builder function compiles the code and returns the compiled function.

This is not good in terms of development, since you no longer able to do C-c C-c a defun.

# option 2 (current design)

Put the dependent code in a separate directory.

Calling the builder function compiles the target file.

During development, I can directly work on the target file with C-c C-c and compile the code, except that
the code does not receive the special optimization based on the instance information.

## Idea: Implement a module-like system based on packages?

--- Nah, setting up a new package (importing/exporting) is annoying.
In fact, there is no need for encupsulation and namespace separation.
The only thing we need is a segmented compilation unit.

Do we REALLY need a segmented compilation unit?

--- Well, NO.

If this repository gets REALLY big, compiling the entire instance-dependent code may take some time.

However, right now this is not the case. Compilation time would not be a problem.

