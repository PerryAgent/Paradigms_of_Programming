(*Q1.*)
datatype Bar = Bar of int
datatype Expr = Const of real
	       |Var of string
	       |Add of Expr*Expr
	       |Mul of Expr*Expr

(*Q2.*)
				
datatype Stmt = Asgn of string*real
	       |Prt of Expr
			   
(*Q3.*)

(*
val eval : Expr -> Env -> real option
val execute : Stmt -> Env -> Env
 *)
			   

type Env = real AtomMap.map
		
fun eval (Const(a)) b = SOME(a)
  | eval (Var(a)) b = AtomMap.find(b,Atom.atom(a))
  | eval (Add(a,b)) c =
    let
	val d = eval a c
	val e = eval b c
	fun int (SOME(f)) (SOME(g)) = SOME(f+g)
	  | int _ _ = NONE
    in
	int d e
    end
  | eval (Mul(a,b)) c =
    let
	val d = eval a c
	val e = eval b c
	fun int (SOME(f)) (SOME(g)) = SOME(f*g)
	  | int _ _ = NONE
    in
	int d e
    end


	
fun execute (Asgn(a,b)) c = AtomMap.insert(c,Atom.atom(a),b)
  | execute (Prt(a)) b =
    let
	val c = eval a b
	fun int (SOME(d)) = print(Real.toString(d))
	  | int (NONE) = print("Nothing to print")
	val e = int c
    in
	b
    end


(*Q4.*)

(*
val evalhash : Expr -> Env -> real option
val executor : Stmt -> Env -> ()
*)
	
type Env = real AtomTable.hash_table

fun evalhash (Const(a)) b = SOME(a)
  | evalhash (Var(a)) b = AtomTable.find b (Atom.atom(a))
  | evalhash (Add(a,b)) c =
    let
	val d = evalhash a c
	val e = evalhash b c
	fun int (SOME(f)) (SOME(g)) = SOME(f+g)
	  | int _ _ = NONE
    in
	int d e
    end
  | evalhash (Mul(a,b)) c =
    let
	val d = evalhash a c
	val e = evalhash b c
	fun int (SOME(f)) (SOME(g)) = SOME(f+g)
	  | int _ _ = NONE
    in
	int d e
    end

fun executor (Asgn(a,b)) c =
    let
	val d = AtomTable.insert c (Atom.atom(a),b)
    in
	()
    end
  | executor (Prt(a)) b =
    let
	val c = evalhash a b
	fun int (SOME(d)) = print(Real.toString(d))
	  | int (NONE) = print("Nothing to print")
	val e = int c
    in
	()
    end

	
	
	
