# Lab 3 Writeup

## 2a)

```
const y = 7
const z = function z(y) { return y - 6}
const x = true + y //Should be 8
z(y) + x //Should be 8 as well
```
```
val z = parse(
"const y = 0;
const x = y + 9;
const e = function e(a){return x};
const f = function f(b){const x = y+2;
 return e(0)};
 f(3)"
 )
```

eval(empty, z) gives us res7: jsy.lab3.ast.Expr = N(2.0)

iterateStep(z) gives us res8: jsy.lab3.ast.Expr = N(9.0)

Under dynamic scoping, variables seem to ignore local scoping under functions, and variable definitions are influenced by variables of the same name under local scoping of functions

Under static scoping, variables are contained by their scopes, and do not affect the definition of variables outside of their respective scope


## 3d)

the judgement form e -> e' is deterministic because there is only one thing that it could evaluate to. Using an example judgement form SearchUnary
```
e1 -> e1'
---------
uope1 -> uope1'
```

e1 is evaluated until it becomes a value. In this case it is left associative since it is evaluated left to right.

## 4. Evaluation Order.

Consider the small-step operational semantics for JAVASCRIPTY shown in Figures 7, 8, and 9. What is the evaluation order for e1 + e2? Explain. How do we change the rules obtain the opposite evaluation order?

First e1 gets evaluated by SearchBinary, then e2 gets evaluated by SearchBinaryArith. So this expression is evaluated left to right. To change the rules, simple have SearchBinary evaluate e2 first, then SearchBinaryArith to step on e2



## 5. Short-Circuit Evaluation. In this question, we will discuss some issues with short-circuit evaluation.

### (a) Concept. Give an example that illustrates the usefulness of short-circuit evaluation. Explain your example.

An example of a useful short-circuit evaluation is the DoAnd. No actual AND operation is necessary, as Javascripty simply checks if v1 is true, then returns e2. If e2 is false, then the expression evaluates to false. This is a very elegant computation as less comptuting power is used.


### (b) JAVASCRIPTY. Consider the small-step operational semantics for JAVASCRIPTY shown in Figures 7, 8, and 9. Does e1 && e2 short circuit? Explain.


Yes, e1 && e2 short circuit, given that e1 is evaluated by SearchBinary first. Once e1 is evaluated to v1, then the following judgement form of DoAND applies
```
true = toBoolean(v1)
---------------------
v1 && e2 => e2
```
