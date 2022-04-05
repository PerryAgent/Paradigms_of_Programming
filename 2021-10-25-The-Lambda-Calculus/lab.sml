(*Q1.*)

type var = string
datatype expr = vari of var
	       |appl of expr*expr
	       |abst of var*expr 
			    

val a = abst("a",abst("b",vari("c")))

(*Q2.*)
open Atom
structure StringKey : ORD_KEY = struct
type ord_key = string
val compare = String.compare
end

structure StringSet : ORD_SET = RedBlackSetFn(StringKey)
				   
(*
val free = fn : expr -> StringSet.item list
*)

fun free (vari(a))   = StringSet.listItems(StringSet.singleton(a))
  | free (appl(a,b)) = StringSet.listItems(StringSet.union(StringSet.fromList(free(a)),StringSet.fromList(free(b))))
  | free (abst(a,b)) = StringSet.listItems(StringSet.difference(StringSet.fromList(free(b)),StringSet.fromList(free(vari(a)))))

val freelist = free(abst("x",appl(vari("x"),vari("y"))))
		   
  
			 
(*Q3.*)
							 
(*
val subst = fn : var * expr -> expr -> expr
*)

fun subst (x,N) (vari(M))   = if x = M then N
			      else vari(M)
  | subst (x,N) (appl(a,b)) = appl((subst(x,N) a),(subst(x,N) b))
  | subst (x,N) (abst(y,e)) = if x = y then abst(y,e)
			         else (abst(y,(subst (x,N) e)))
				       

val v = "x"
val f = abst("x",vari("y"))
				     
val substval = subst (v,f) (vari("y"))
		     
