Each function accepts monomials and polynomials both in traditional
and in parsed form as input. 
Example structures:

Monomial in traditional form:    (* 5 (expt y 3))
Monomial in parsed form:         (M 5 3 ((V 3 Y)))
Polynomial in traditional form:  (+ 3 (* 3 (expt x 2)))
Polynomial in parsed form:       (POLY ((M 3 0 NIL) (M 3 2 ((V 2 X)))))

The functions as-monomial and as-polynomial required a monomial and a polynomial
in traditional form respectively. They do not support objects in parsed form. 
The input is parsed, normalised and ordered lexicographically. 

The functions polyplus, polyminus, polytimes accept any kind of input in a pairwise
combination, but only if both objects adhere to the same structure (both in traditional
form or both in parsed form). Unsupported combinations produce NIL as result. 
Null values are interpreted as 0 (zero).

It is assumed that parsed monomial and polynomial given as input are correct, eventually
not normalised and unordered. These operations are executed automatically by each function.

The value 0 (zero) is represented stand-alone in the following form:
(M 0 0 NIL)
If the value 0 (zero) is included within a Polynomial, it is ignored. 

Any variable raised to the power 0 (zero) are automatically interpreted as 1.

Traditional form ---> Parsed form :
0   --->  (POLY ((M 0 0 NIL)))
(+ x 0)  --->  (POLY ((M 1 1 ((V 1 X)))))
(+ (* 3 x) (* -3 x) (* 2 x))  --->  (POLY ((M 2 1 ((V 1 Y)))))
(+ (* 3 x 0) 2) --->  (POLY ((M 2 0 NIL)))
(expt x 0)  --->  (POLY ((M 1 0 NIL)))
(+ (* 3 (expt x 0)) 2) --->  (POLY ((M 5 0 NIL)))
(* (expt x 2) (expt x -2) a) --->  (POLY ((M 1 1 ((V 1 A)))))
