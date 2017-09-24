## Exercise 3 ##
Time taken 2.5 hours for the initial solution; 0.5 more for the optimisation.

### Task ###

The lecture notes of this week discuss the conversion of Boolean formulas (formulas of propositional logic) into CNF form. The lecture notes also give a definition of a Haskell datatype for formulas of propositional logic, using lists for conjunctions and disjunctions. Your task is to write a Haskell program for converting formulas into CNF.

### Deliverables ###
 
- conversion program with documentation;
- indication of time spent.

## Completed exercise ##

The conversion program is defined in the file `Exercise_3.hs`.

The function `cnf :: Form -> Form` translates any `Form` into its *Conjunction Normal Form* by constructing equivalent representations in CNF of all rows evaluating to `False`.

**Note**

When the Form represents a **Tautology** no such rows will be found which leads to the simplest of all CNF forms:

 `*()` (or `Cnj []` in type-language). 
 
This (with behindsight logical) observation led me to find a possibility to optimise the CNF dramatically in case the Form is a **Contradiction**. In that case **all** rows will satisfy the Form, thus the CNF of the Form is just 

`Dsj []` (or `+()` in CNF-language). 

N.B. Although `Dsj []` at first sight could perhaps be seen as not being in CNF-form, in fact it is, because it is a so-called *simple term* [1].

[1] [From Wikipedia, the free encyclopedia: **Conjunctive normal form**](https://en.wikipedia.org/wiki/Conjunctive_normal_form)

