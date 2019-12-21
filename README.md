# Xcuuuse
Proof checker

how to build: `ghc --make -shared -fPIC Xcuuuse.hs -o xcuuus.so`

## Grammar

*Formula* ::= *Variable* | ( *Formula* ) | *Negation* | *Conjunction* | *Disjunction* | *Implication* | *Equivalence*  
*Variable* ::= *Uppercase* | *Variable* *Num* | *Variable* '  
*Negation* ::= *Negsym* *Formula*  
*Conjunction* ::= *Formula* *Consym* *Formula*  
*Disjunction* ::= *Formula* *Dissym* *Formula*  
*Implication* ::= *Formula* *Impsym* *Formula*  
*Equivalence* ::= *Formula* *Eqsym* *Formula*  

*Num* ::= 0|...|9  
*Uppercase* ::= A|...|Z  
*Negsym* ::= - | ~  
*Consym* ::= & | /\  
*Dissym* ::= v | \\/  
*Impsym* ::= -> | =>  
*Eqsym* ::= = | <=>  
