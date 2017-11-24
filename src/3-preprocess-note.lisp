;;; Preprocessing step does invariant synthesis and grounding.

(in-package :strips)

#|
Preprocessing steps.

I want to solve this step using an external solver (SAT, Prolog).
I briefly summarize the invariant synthesis/grounding module in Fast Downward below.
|#

;;; Invariant Synthesis

#|

FD uses finite-domain representation (SAS).
To generate SAS, FD requires mutex invariants.
(mutex groups: set of propositions where only 0 or 1 element is true.)
(exactly-1 invariants (Vidal and Torralba): groups where exactly 1 element is true.)
(mutex groups become exactly-1 invariants by adding a "none-of-the-above".)

** Monotonicity invarinant candidate

(?p: place, ?l : location, ?v : vehicle)

candidate: (:parameters (?p) :atoms ((at ?p ?l) (in ?p ?v)))

counted variables: free variables ?l, ?v. 

For all ?p, #(at ?p ?l) + #(in ?p ?v) is constant or decreasing.

** Monotonicity Invariants

We can instantiate a candidate: ?p -> package1, ((at package1 ?l) (in package1 ?v))

Covered facts: instantiations of free variables. (at package1 loc1), (at package1 loc2), (in package1 truck)

Weight: the number of covered facts. 3. (but we are only interested in weight 1)

The weight shuold be constant or decreasing on any states.

** 5.1 breadth-first search
*** 1. ignore static predicates.
*** 2. ignore derived predicates.                                  --- Invariants in derived predicates are rare. 
*** 3. picks the candidates with one counted-var and one atom.     --- 2+ counted-var is rare.
Examples:
  ((?x)    (at ?x ?l))
  ((?l)    (at ?x ?l))
  ((?x ?l) (at ?x ?l)) 
  (()   (at ?x ?l)) XXX two counted variables.

  ((?y) (in ?y ?v))
  ((?v) (in ?y ?v))
  (()   (in ?y ?v))

** 5.2 prove invariance: Check if an invariance is threatened.
Invariance is proved when all operators are balanced and not too heavy:
Balanced: +1, -1
Too heavy: increase the weight more than twice. (ignore delete effects). Thus, +2,-2 or +3,-3 are not allowed
Too heavy condition is necessary because one delete could be shared by two adds.

Precondition: at(x, l1)  and  at(x, l2)
Add effects: at(x, l3)  and  at(x, l4)
Delete effects: at(x, l1)  and  at(x, l2)      --- This increases the weight when l1=l2 and l3!=l4.

Precondition: at(x, l1)  and  at(x, l2) and not(equal(l1,l2))
Add effects: at(x, l3)  and  at(x, l4)
Delete effects: at(x, l1)  and  at(x, l2)
      --- This maintains the weight=2, but we don't care the invariance with weight>=2 (we care mutex only: weight=1)


So, taking the heaviness test for example,

- initially

operator params: ?z
operator precond: (baz ?z)
e: (forall (?x) (when (foo ?x ?y) (bar ?x ?z)))
e.cond: (foo ?x ?y)
e.atom: (bar ?x ?z) - (atomic formula, not lisp/prolog-type atom=symbol)

- quantified var ?x is assigned a unique name, the effect is duplicated

e:  (forall (?a) (when (foo ?a ?y) (bar ?a ?z)))
e': (forall (?b) (when (foo ?b ?y) (bar ?b ?z)))

phi1: (bar ?x ?l), counted: ?l
phi2: (bar ?x ?l), counted: ?x
phi3: (bar ?x ?l), counted: none
phi4: (bar ?x ?l), counted: ?l, ?x -- >2 counted vars, not included

- now cover phy for e.atom:

bound symbols: ?x, ?a and ?z, which are all universally quantified (p22, Even if ... overly conservative)

There are no bar instance in precond nor e.cond, so they can be ignored

phi1: ?x cannot be aliased to ?a because ?x is already bound
phi2: ?l cannot be aliased to ?z because ?l is already bound
phi3: Unification {?x = ?a, ?l = ?z} exists





Note: does weight=2 invariant reduces the state encoding?

one-hot vector of length N -> log_2 N bit
two-hot vector of length N -> log_2 N(N-1)/2 bit

two-hot vector encoding: first N bits resignate the first element X. the remaining bits represents the delta.
If X+delta >= N, subtract N: X+delta - N. Delta requires only N-1 bit.

** 5.3 Refine the candidates: successors.
Too heavy -> rejected immediately.
Inbalanced: -> add a new atom from the delete effects.
Do not consider when there are multiple counted variables.
|#

;;; Grounding.

#| 

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



|#

#| Generating FDR.

Select some mutex group so that all atoms are covered.
This is a set-covering problem (NP complete), and in particular not APX (constant-ratio approximable).
Thus FD uses a greedy algorithm with O(logn) approximation.

|#
