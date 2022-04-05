(*Q1.*)

(*
Creating a structure with the following signature to expose given functions
*)
signature Counter = sig
    val incr : unit -> unit
    val decrement : unit -> unit
    val get : unit -> int
end
			
structure Ct : Counter
= struct
  type t = int ref
  val c = ref 0
  fun incr () = c := (!c) + 1
  fun decrement () = c:= (!c) -1
  fun get () = !c    
end

val _ = Ct.incr()
val _ = Ct.incr()
val _ = Ct.incr()
val _ = Ct.decrement()
val c = Ct.get()
				
				
(*Q2.*)	

(*
Creating a functor with the Signature given above to be able to create multiple similar structures
*)		     
functor MkCounter () : Counter
= struct
   type t = int ref
   val c = ref 0
   fun incr () = c := (!c) + 1
   fun decrement () = c:= (!c) -1
   fun get () = !c    
end

structure A = MkCounter()
structure B = MkCounter()

val _ = A.incr()
val _ = B.incr()
val _ = B.incr()
val _ = A.decrement()
val x = A.get()
val y = B.get()
	     

signature SIG = sig

    type foo
    val func : foo -> foo

end

structure A : SIG = struct

  type foo   = int

  val y      = 42

  fun func x = x + y

end

signature SIG = sig
    type bar
    val public : bar -> bar
end
		    
			    
structure A : SIG = struct

  type bar = int

  fun public  (x : bar) = x

  fun private (x : bar) = x + 1

end
			
structure A = struct

  type foo   = int

  val y      = 42

  fun func x = x + y

end
 (*
signature BSIG = sig

  type foo

  type bar = int

  val  tobar : foo -> bar

  val  tofoo : bar -> foo

end



structure B :> BSIG = struct

  type foo     = int

  type bar     = int

  fun  tobar x = x

  fun  tofoo x = x

end
*)(*			  
fun foo1 (x : B.foo) = B.tobar x + 1

fun foo2 (x : B.foo) = x + 1

fun foo3 (x : B.bar) = x + 1

fun foo4 (x : B.bar) = B.tofoo x + 1


signature SIG = sig
      type bar = int
      type foo 
end
    
structure A : SIG = struct

  type bar = int

  type foo = int

end
*)
		    

