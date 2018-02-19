
## Architecture

Alien planner has several stages of source code.

+ Preprocessing phase
  + PARSE           --- parse the PDDL into canonical representation (similar to FD)
  + EASY-INVARIANT  --- detect some trivial invariants
  + GROUND          --- perform reachability analysis using B-PROLOG
  + MUTEX-INVARIANT --- named inadequately. performs axiom-layer computation using B-PROLOG
  + INSTANTIATE     --- Set up SG and other helpful structures.
+ Search phase
  + Generic code            --- Part of the code that does not depend on the instance information.
                                Not much actual search code.
                                Consists of the code that calls the preprocessor,
                                compiles the instance-dependent code,
                                open list code (which does not depend on instances),
                                logging / memory management utility etc.
  + Instance-dependent code --- The source code that is recompiled after the instance information is available.
                                This includes search engines, state manipulators and heuristic functions.

## Recompilation of instance-dependent code

The memory limit is defined in the generic code. Instance-dependent code is compiled
three times with different read-time conditionals.

+ 1st compilation: this happens ahead-of-time, when the alien planner is built in the first time.
+ 2nd compilation: this compiles the `define-packed-struct` macros in the source code.
  + `define-packed-struct` is a macro that defines a C-like structure/bitfield and its constructor/accessor functions.
    It defines the following functions:
    + Constructor for a single instance.
    + Allocator for a large bit-array in a consecutive region.
    + Accessors to the slots in a `packed-struct`.
      These accessors use SBCL internal functions that reads an array by *word*,
      bypassing the standard API for bit arrays.
      The result is further optimized by embedding the static offset information for each slot.
  + The size of each packed struct slot is computed using the instance information. For example,
    + When there are N fluent propositional variables in the instance,
      state information consumes exactly N bit in the resulting structure.
    + You know the range of heuristic values.
      For example, goal-count heuristics always returns a number smaller than the number of fluents (incl. axioms).
      We use the minimal number of bits to cache the heuristic values in a state information.
    + This also gives you the range of `state-id` integer type,
      since you know exactly how much space is consumed by each state and
      also how much memory is available.
    + Similarly for other types such as `op-id`, which is equal to the number of operators.
    + Procedures depending on a successor generator (applicable-ops, apply-op,
      apply-axioms) are statically compiled into an assembly sequence.
+ 3rd compilation: the source code for the searcher, heuristics etc. are
  recompiled using the types derived in the second compilation.

There is no call to `malloc` in a sense of C/C++, since SBCL allocates a
consecutive memory array when it launches itself. Everything (incl. GC) is a large array after all.
When you need to allocate something, you simply increments a pointer in the array.


