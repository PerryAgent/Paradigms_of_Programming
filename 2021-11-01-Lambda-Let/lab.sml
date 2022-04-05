(*Q1.*)

type var = string
	       
datatype llet = letvar of var
	       |letapp of llet*llet
	       |letabs of var*llet
	       |letlb of var*llet*llet
				      
(*

letlb of var*llet*llet    ====> let    x = e1 in e2 end
letrlb of var*lletr*lletr ====> letrec x = e1 in e2 end
 
*)

datatype lletr = letrvar of var
	        |letrapp of lletr*lletr
	        |letrabs of var*lletr
		|letrlb of var*lletr*lletr
					 

(*Q2.*)

(* lambda-calculus datatype *)

datatype l = lvar of var
	    |lapp of l*l
	    |labs of var*l
				    
(* Conversion function *)

fun llet_l (letvar(a))      =  lvar(a)
  | llet_l (letapp(a,b))    =  lapp(llet_l(a),llet_l(b))
  | llet_l (letabs(x,e))    =  labs(x,llet_l(e))
  | llet_l (letlb(x,e1,e2)) =  lapp(labs(x,llet_l(e2)),llet_l(e1))
			      

    
				       
				
