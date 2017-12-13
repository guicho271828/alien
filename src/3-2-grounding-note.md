# Grounding.

Section 6.1.1, 6.1.2, 6.1.3 describes the previous work.
The actual grounding is performed by solving a Datalog program.

The resulting Datalog program is converted to a "normal" rules.
The normal rules consists of projection rules and join rules, but they basically means that
 the body of a normal rule conists of only two facts.
The original datalog rule is decomposed into a set of normal rules.

This decomposition is equivalent to conjunctive query optimization for databases.
Join operation (cartesian product) should be performed as late as possible.


Whethere we should implement this by hand (as FD did) or by calling a prolog solver (e.g. SWI-prolog) remains unknown.

Another way to do this is simply avoid this stage. This is possible when we use ZDD solver, since
nonexistent element consume no resource in ZDD.

# Generating FDR.

Select some mutex group so that all atoms are covered.
This is a set-covering problem (NP complete), and in particular not APX (constant-ratio approximable).
Thus FD uses a greedy algorithm with O(logn) approximation.

