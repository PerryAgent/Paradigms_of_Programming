(* Q1. *)

fun foldr _ x [] = x
  | foldr f x (y::ys) = f(y,(foldr f x ys))
			 
fun foldl _ x [] = x
  | foldl f x (y::ys) = foldl f (f(y,x)) ys

(*
val foldr = fn : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary

val foldl = fn : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary
*)

val list = [1,2,3,4,15]	     

(* Q2. *)
			      	       
(*
val sum = fn : int list -> int
*)
			
fun sum x = foldl (fn (u,v) => u+v) 0 x (* Function that uses folding to compute sum of list of integers *)
				
val add = sum list

(* Q3. *)

(*
val partition = fn : ('a -> bool) -> 'a list -> 'a list * 'a list
val g = fn : int -> bool
*)
		 
fun partition g l = ((foldr (fn (u,v) => if (g u) then u::v else v) [] l),(foldr (fn (u,v) => if not (g u) then u::v else v) [] l))

fun g a = if ((a mod 2)=0) then true else false

val part = partition g list (* ([2,4],[1,3,15]) *)

(*
val map = fn : ('a -> 'b) -> 'a list -> 'b list
val f = fn : int -> int 
*)		     			 		  		     

fun map f l = foldr (fn (u,v) => f(u)::v) [] l

fun f a = a+1
		
val mapped = map f list (* [2,3,4,5,16] *)

(*		 
val reverse = fn : 'a list -> 'a list
*)

fun reverse l = foldl (fn (u,v) => u::v) [] l
				     		      
val r = reverse list (* [15,4,3,2,1] *)

(*
val nthAux = fn : 'a * 'a Find -> 'a Find
val convert = fn : 'a Find -> 'a option

val nth = fn : 'a list * int -> 'a option
*)
datatype 'a Find = LookingFor of int
		 | Found of 'a
		       	       
			
fun nthAux (x,LookingFor(n)) = if (n = 0) then Found(x) else LookingFor(n-1)
  | nthAux (_,Found(x)) = Found(x)

fun convert (Found(x)) = SOME x
  | convert (LookingFor(x)) = NONE 
					 
fun nth (l,n) =
    let 
	val ans = foldl nthAux (LookingFor(n)) l
    in
	convert ans
    end;

val n = nth (list,3) (* SOME 4 *)
val n1 = nth (list,6) (* NONE *)
	     
datatype 'a option = SOME of 'a
       | NONE
	     


		 
		      

	



											      
		      
		      
  

			    
  
			  

						     
		    
		
		
		      

