                              (* Q1. *)

(* val tri_curry = fn : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd *)

fun tri_curry f a b c = f(a,b,c)

fun e (a,b,c) = a*b*c
val f = tri_curry e 1 2 3;
		 	 
(* val tri_uncurry = fn : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd  *)
			 
fun tri_uncurry f (a,b,c) = f a b c
fun g a b c = a*b*c
val h = tri_uncurry g (2,3,4);

			      (* Q2. *)
fun fst (a,b) = a
		    (* Returns first element of tuple  *)
fun snd (a,b) = b
		    (* Returns second element of tuple *)
val x = fst(2,7);
val y = snd(2,7);
	   
		    
               		      (* Q3. *)
fun length (a) =
    if null(a) then 0
    else 1+length(List.drop(a,1))
		 (* Recursively calls itself, everytime deleting the first element of list till list becomes nil *)
val len = length([9,7,4,8]);

fun length2 ([]) = 0
  | length2 (x::ay) = 1 + length2 (ay)
				  

				


		              (* Q4. *)
fun reverse a =
    if null(a) then a
    else reverse(List.drop(a,1)) @List.take(a,1)
(* 
Recursively calls itself, everytime deleting first element of list and reverse concatenating that to the reverse return 
*)

fun rev2 [] = []
  | rev2 (x::ay) = (rev2 ay)@[x]
				    
					   
val rl = reverse [4,11,8,9];
		

			      (* Q5. *)

fun fib a =
    let
	fun fibi (1,b,c) = c
	  | fibi (2,b,c) = c
	  | fibi (a,b,c) = fibi (a-1,c,b+c)
    in
	fibi(a,1,1)
    end;
(* This method calculates fibonacci in O(n) *)

val nf = fib 14

(* The below method also calculates fibonacci but its time complexity is exponential*)

fun alt_fib a = if (a=1 orelse a =2) then 1
		else alt_fib(a-1) + alt_fib(a-2)
val nf_a = alt_fib 14

		      


				   

       

				      
				    
		 

		   
					   
	    
fun len2 [] = 0
  | len2 h::t  = 1 + (len2 t)  
				
		 
		 
fun fib n =
    let
	

    fun fx 1 a b = b
      | fx 2 a b = b
      | fx n a b = fx (n-1) b (a+b)

    in
	fx n 1 1
    end
	

		     
				  
	

		   
		    
		    
		

			      
			      
