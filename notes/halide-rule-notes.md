# Halide rule notes (scratch)

The point of this is to get a sense for what rules are important -- this will guide
which things we have to tackle next (facts with free vars on RHS, scheduling,
monotonicity templates, interval analysis, etc.).

R1:
x * c0 < y * c0 ==>  x < y if  c0 > 0

Derivation: We will rewrite the LHS into the RHS.

x * c0 < y * c0           | 
(y * c0 - x * c0) > 0     |  a < b ~> (b - a) > 0
c0 * (y - x) > 0          |  x * z - y * z ~> z * (x - y)
y - x > 0                 |  if b > 0 then a * b > c ~> a > c / b 
x < y                     | (b - a) > 0 ~> a < b

Notes:
Seems like the the fundamental rules to discover first are
monotonicity of multiplication over less than, and ways of
moving constants/operations through comparison operators.

R2:
x * c0 < y * c0 ==>  y < x if  c0 < 0

Derivation:
We will rewrite the LHS into the RHS.
x * c0 < y * c0           | 
(y * c0 - x * c0) > 0     |  a < b ~> (b - a) > 0
c0 * (y - x) > 0          |  x * z - y * z ~> z * (x - y)
y - x < 0                 | if b < 0 then a * b > c ~> a < c / b
y < x                     | (b - a) < c ~> b < a + c

Notes:
Seems like the the fundamental rules to discover first are
monotonicity of multiplication over less than, and ways of
moving constants/operations through comparison operators.

R3:
x / c0 < c1 ==>  x < c1 * c0 if  c0 > 0

Derivation:
We will rewrite the LHS into the RHS.
x / c0 < c1               | 
x < c1 * c0               | x / c0 < c1 ~> x < c1 * c0 if c0 > 0

Notes:
This is a one-shot rule derivation; the rule itself _is_ a fundamental axiom.


