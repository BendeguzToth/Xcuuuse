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
**Modus tollens**  
![](rules/mt.png)  
**Disjunction introduction**  
![](rules/disjunction_i.png)  
**Disjunction elimination**  
![](rules/disjunction_i.png)  
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

*Proof* ::= (*Premise*)\* *Conclusion* (*Derivation*)\+  
*Premise* ::= *Natural* *Formula* p  
*Conclusion* ::= *Thensym* *Formula*  
*Derivation* ::= *Natural* *Scope* *Formula* *Justification*  
*Scope* ::= ('|')\* ('\*')?
*Justification* ::= *Symbol* *RuleType* *Reference*  
*Reference* ::= *Natural* | *Range* | *Natural* , *Reference* | *Range* , *Reference*  
*Formula* ::= *Variable* | ( *Formula* ) | *Negation* | *Conjunction* | *Disjunction* | *Implication* | *Equivalence* | *Contradiction*  
*Variable* ::= *Uppercase* | *Variable* *Num* | *Variable* '  
*Negation* ::= *Negsym* *Formula*  
*Conjunction* ::= *Formula* *Consym* *Formula*  
*Disjunction* ::= *Formula* *Dissym* *Formula*  
*Implication* ::= *Formula* *Impsym* *Formula*  
*Equivalence* ::= *Formula* *Eqsym* *Formula*  
*Contradiction* ::= #  
*Symbol* := *Negsym* | *Consym* | *Dissym* | *Impsym* | *Eqsym*  


*Num* ::= 0|...|9  
*Natural* ::= 1|...|9 | *Natural* *Num*  
*Range* ::= *Natural* - *Natural*  
*Uppercase* ::= A|...|Z  
*Negsym* ::= - | ~  
*Consym* ::= & | /\  
*Dissym* ::= v | \\/  
*Impsym* ::= -> | =>  
*Eqsym* ::= = | <=>  
*Thensym* ::= '|-'  
*RuleType* ::= i | e
