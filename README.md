# Xcuuuse
Proof checker

how to build: `ghc --make -shared -fPIC Xcuuuse.hs -o xcuuus.so`

## List of rules

List of rules supported by Xcuuuse. The number and types (single line or range of box) are listed. The order of references does not matter, as long as they are of the correct type.

**Conjuntion introduction**  
![](rules/conjunction_i.png)  
**Conjunction elimination**  
![](rules/conjunction_e.png)  
**Double negation introduction**  
![](rules/dn_i.png)  
**Double negation elimination**  
![](rules/dn_e.png)  
**Implication introduction**  
![](rules/implies_i.png)  
**Implication elimination (modus ponens)**  
![](rules/implies_e.png)  
**Equiavlence introduction**  
**Equiavlence elimination**  
**Modus tollens**  
![](rules/mt.png)  
**Disjunction introduction**  
![](rules/disjunction_i.png)  
**Disjunction elimination**  
![](rules/disjunction_e.png)  
**Reiterate**  
![](rules/reiterate.png)  
**Contradiction elimination**  
![](rules/contradiction_e.png)  
**Negation introduction**  
![](rules/negation_i.png)  
**Negation elimination**  
![](rules/negation_e.png)  
**Proof by contradiction (reductio ad absurdum)**  
![](rules/pbc.png)  
**Law of excluded middle (tertium non datur)**  
![](rules/lem.png)  


## Grammar
The grammar specification of a `.proof` file, using EBNF notation. The terminals are enclosed within `""`. Square brackets denote the scope of an operator (`*`, `+` or `?`).

```
Proof         ::= [Premise]* Conclusion [Derivation]+
Premise       ::= Natural Formula "p"
Conclusion    ::= Thensym Formula
Derivation    ::= Natural Scope Formula Justification
Scope         ::= ["|"]* ["|*"]?
Justificatoion::= Consym "i" Reference Reference
                  | Consym "e" Reference
                  | Negsym Negsym "i" Reference
                  | Negsym Negsym "e" Reference
                  | Impsym "i" Reference
                  | Impsym "e" Reference Reference
                  | Eqsym "i" Reference Refrence
                  | Eqsym "e" Reference
                  | MTsym Reference Reference
                  | Dissym "i" Reference
                  | Dissym "e" Reference Reference Reference
                  | Resym Reference
                  | Contsym "e" Reference
                  | Negsym "i" Reference
                  | Negsym "e" Reference Reference
                  | Pbcsym Reference
                  | Lemsym
                  | Asssym
Reference     ::= Natural | Range | Natural , Reference | Range , Reference

Formula       ::= Variable | ( Formula ) | Negation | Conjunction | Disjunction 
              | Implication | Equivalence | Contradiction  
Variable      ::= Uppercase | Variable Num | Variable
Negation      ::= Negsym Formula
Conjunction   ::= Formula Consym Formula
Disjunction   ::= Formula Dissym Formula
Implication   ::= Formula Impsym Formula
Equivalence   ::= Formula Eqsym Formula
Contradiction ::= Contsym

Num       ::= "0"|...|"9"  
Natural   ::= "1"|...|"9" | Natural Num
Range     ::= Natural - Natural
Uppercase ::= "A"|...|"Z"
Negsym    ::= "-" | "~"
Consym    ::= "&" | "/\"
Dissym    ::= "v" | "\/"
Impsym    ::= "->" | "=>"  
MTsym     ::= "mt"
Resym     ::= "r"
Contsym   ::= "#"
Pbcsym    ::= "pbc"
Lemsym    ::= "lem"
Eqsym     ::= "=" | "<=>"
Thensym   ::= "|-" 
Asssym    ::= "ass"
```
