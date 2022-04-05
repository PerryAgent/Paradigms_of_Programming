(* Q1. *)
	    
				
fun f a = a+1

		
fun map f []  = []
  | map f (a::b) = (f a) :: (map f  b);

val mp = map f [4,3,8]
	
	
(* Q2. *)

datatype 'a tree = emp | nd of 'a tree * 'a * 'a tree

val t = nd(nd(nd(nd(emp,12,emp),15,emp),5,nd(emp,27,emp)),2,nd(emp,7,emp))
	  

(*

			2
		       / \
		      5   7
		     / \
		    15  27
		   /
		  12

*)
							   


(* Q3. *)

(*
val treemap = fn : ('a -> 'b) -> 'a tree - > 'b tree
*)
					     
fun treemap f emp = emp 
  | treemap f  (nd(l,x,r)) = nd((treemap f l), (f x), (treemap f r));
				
val mpt = treemap f t

(* Q4. *)
		  
		  (* val inorder =  fn : 'a tree -> 'a list *)

fun inorder emp = []
  | inorder (nd(l,x,r)) = inorder(l) @ [x] @ inorder(r)

val ino = inorder t

		  (* val preorder = fn : 'a tree -> 'a list *)

fun preorder emp = []
  | preorder (nd(l,x,r)) = [x] @ preorder(l) @ preorder(r)

val preo = preorder t

                  (* val postorder = fn : 'a tree -> 'a list *)

fun postorder emp = []
  | postorder (nd(l,x,r)) = postorder(l) @ postorder(r) @ [x]

val posto = postorder t
		      
(* Q5. *)		    
		  
fun rotate emp = emp
  | rotate (nd(nd(lc,c,rc),x,r)) = nd(lc,c,nd(rc,x,r))
  | rotate (nd(emp,x,r)) = nd(emp,x,r)

val rot = rotate t

val rot1 = rotate (nd(emp,2,emp))

datatype int a option = SOME of a
       | NONE
	     

		    
