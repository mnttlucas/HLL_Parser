open Ast
open List
open Printf
open Processing
open Sys
open Unix
open Utils

(** [Translater] module generates SMT formulas from an HLL file.
	*)

let fop_smt = function
	| FDabs -> "abs"
	| FDmin -> "_hll_min"
	| FDmax -> "_hll_max"
	| _ -> "NIY"

let quantif_smt = function
	| QAll -> "forall"
	| QConj -> "forall"
	| QDisj -> "exists"
	| QSome -> "exists"
	| _ -> "NIY"

let unop_smt = function
	| UMinus -> "-"
	| UNot -> "not"

let binop_smt = function
	| BAnd -> "and"
	| BDifference -> "-"
	| BDifferent -> "distinct"
	| BDivision -> "div"
	| BEqual -> "="
	| BEquivalence -> "="
	| BGreater -> ">"
	| BGreater_Eq -> ">="
	| BImplication -> "=>"
	| BLess -> "<"
	| BLess_Eq -> "<="
	| BModulo -> "mod"
	| BOr -> "or"
	| BProduct -> "*"
	| BSum -> "+"
	| _ -> "NIY"

let rec type_smt = function
	| TBool		-> "Bool"
	| TInt(_)	-> "Int"
	| TPath(PAbsolute(p)) -> String.concat "::" p
	| TPath(PRelative(p)) -> String.concat "::" p
	| TStructure(sl) -> sprintf "(str%d %s)" (length sl) (String.concat " " (map (fun e ->  (type_smt (snd e))) sl))
	| TArray(ty, [EClosed(CeIntLit(i), [])]) -> sprintf "(str%s%s)" i (rep (int_of_string i) (type_smt ty) "")
	| _			-> "NIY"

let rec quantif_var_list_smt l acc = 
	match l with
		| [] -> acc
		| [QvDomain(s, _)] -> quantif_var_list_smt [] (acc ^ sprintf "(%s Int)" s)
		| QvDomain(s, _) :: t -> quantif_var_list_smt t (acc ^ sprintf "(%s Int) " s)
		| _ -> "NIY" (* TODO items*)

let accessors_smt l =
	let rec expr_list_smt l acc =
		match l with
			| [] -> acc
			| EClosed(CePath(PRelative([p])), []) :: t -> expr_list_smt t (acc ^ sprintf " %s" p)
			| EClosed(CeIntLit(s), []) :: t -> expr_list_smt t (acc ^ sprintf " %s" s)
			| _ :: t -> expr_list_smt t (acc ^ " NIY")
	in
	match l with
		| [] -> ""
		| [AArray(a)] -> expr_list_smt a ""
		| [AFunction(f)] -> expr_list_smt f ""
		| [ATuple(s)] -> s (* TODO : implementer les struct & tuple en SMT datatype*)
		| [AStruct(s)] -> s
		| _ -> "NIY"

let rec quantif_expr_smt a v e ql =
	let rec modified_expr ql acc =
		match ql with
			| [] -> sprintf "(and%s)" acc
			| QvDomain(s, DRange(e1, e2)) :: t -> modified_expr t (acc ^ sprintf " (>= %s %s) (<= %s %s)" s (expr_smt a "" false e1) s (expr_smt a "" false e2))
			| _ :: t -> modified_expr t acc
	in
	match ql with
		| l when for_all (fun qv -> match qv with | QvDomain(_, DRange(_, _)) -> false | _ -> true) l -> expr_smt a v false e
		| _ -> sprintf "(=> %s %s)" (modified_expr ql "") (expr_smt a v false e)

and expr_smt a v m = function
	| EClosed(CeTrue, []) -> "true"
	| EClosed(CeFalse, []) -> "false"
	| EClosed(CeIntLit(s), []) when (String.starts_with ~prefix:"0b" s || String.starts_with ~prefix:"0B" s) -> "#b" ^ String.sub s 2 (String.length s - 2)
	| EClosed(CeIntLit(s), []) when (String.starts_with ~prefix:"0x" s || String.starts_with ~prefix:"0X" s) -> "#x" ^ String.sub s 2 (String.length s - 2)
	| EClosed(CeIntLit(s), []) -> s
	| EClosed(CeExpr(e), []) -> expr_smt a v m e
	| EUnOp(u, e) -> sprintf "(%s %s)" (unop_smt u) (expr_smt a v m e)
	| EBinOp(e1, b, e2) -> sprintf "(%s %s %s)" (binop_smt b) (expr_smt a v m e1) (expr_smt a v m e2)
	| EClosed(CePath(PRelative([p])), []) when exists (fun d -> match d with | ProcDc(_, DDeclarator(s, _)) -> s = p) a.declarations
											|| exists (fun i -> match i with | ProcIn(_, IdInit(DDeclarator(s, _))) -> s = p | ProcIn(_, IdName(DDeclarator(s, _))) -> s = p) a.inputs
											-> sprintf "(%s %s)" p v
	| EClosed(CePath(PRelative([p])), []) -> p
	| EClosed(CePath(PRelative([p])), l) -> sprintf "(%s %s%s)" p v (accessors_smt l)
	(*pas de gestion du type : DefUndeclaredType page 92 spec*)
	(* bool m : ind*)
	| EClosed(CeTypedPre2(_, e1, e2), []) ->
		if m
		then
			sprintf "%s" (expr_smt a v m e1)
		else 
			sprintf "(ite (= %s #x0) %s %s)" v (expr_smt a v m e2) (expr_smt a (sprintf "(bvadd %s (bvneg #x1))" v) m e1)
	| EClosed(CeUntypedPre2(e1, e2), []) -> 
		if m
		then
			sprintf "%s" (expr_smt a v m e1)
		else
			sprintf "(ite (= %s #x0) %s %s)" v (expr_smt a v m e2) (expr_smt a (sprintf "(bvadd %s (bvneg #x1))" v) m e1)
	| EClosed(CeFop(f, [e1; e2]), []) -> sprintf "(%s %s %s)" (fop_smt f) (expr_smt a v m e1) (expr_smt a v m e2)
	| EClosed(CeFop(f, [e]), []) -> sprintf "(%s %s)" (fop_smt f) (expr_smt a v m e)
	| EClosed(CeNext(EClosed(CePath(PRelative([i])), l)), []) -> sprintf "(%s (bvadd %s #x1)%s)" i v (accessors_smt l)
	| (* TODO : test it *) EClosed(CeQuantif(QeQuantif(q, ql, qe)), []) -> sprintf "(%s (%s) %s)" (quantif_smt q) (quantif_var_list_smt ql "") (expr_smt a v m (EClosed(CeQuantif(qe), [])))
	| EClosed(CeQuantif(QeExpr(q, ql, e)), []) -> sprintf "(%s (%s) %s)" (quantif_smt q) (quantif_var_list_smt ql "") (quantif_expr_smt a v e ql)
	| (* TODO : this is when Quantif expr is select -> rhs = default value *) EClosed(CeQuantif(QeExprRhs(_, _, _, _)), []) -> "NIY"
	| EIfThenElse(e1, e2, [], e3) -> sprintf "(ite %s %s %s)" (expr_smt a v m e1) (expr_smt a v m e2) (expr_smt a v m e3)
	| _ -> "NIY"

let rec suffix_smt l acc =
	match l with
	| [] -> acc
	| _ :: t -> suffix_smt t (acc ^ " Int")

let rec types_bmc a l acc =
	match l with
		| [] -> acc
		| h :: t ->
		(match h with
			| ProcTEnum(sl, n) -> types_bmc a t (acc ^ sprintf "(declare-datatypes () ((%s %s)))\n" n (String.concat " " sl))
			| ProcTType(TStructure(sl), DDeclarator(s, [])) -> types_bmc a t (acc ^ sprintf "(define-sort %s () (str%d %s))\n" s (length sl) (String.concat " " (map (fun e ->  (type_smt (snd e))) sl)))
			| ProcTType(TInt(IInt), DDeclarator(s, [])) -> types_bmc a t (sprintf "(define-sort %s () Int)\n" s ^ acc)
			| ProcTType(TBool, DDeclarator(s, [])) -> types_bmc a t (sprintf "(define-sort %s () Bool)\n" s ^ acc)
			| ProcTType(TArray(TArray(ty, [EClosed(CeIntLit(i2), [])]), [EClosed(CeIntLit(i1), [])]), DDeclarator(s, [])) -> types_bmc a t (acc ^ sprintf "(define-sort %s () (str%s%s))\n" s i2 (rep (int_of_string i2) (sprintf "(str%s%s)" i1 (rep (int_of_string i1) (type_smt ty) "")) ""))
			| ProcTType(TArray(ty, [EClosed(CeIntLit(i), [])]), DDeclarator(s, [])) -> types_bmc a t (acc ^ sprintf "(define-sort %s () (str%s%s))\n" s i (rep (int_of_string i) (type_smt ty) ""))
			| _ -> types_bmc a t acc)
		
let rec constants_bmc a c acc =
	match c with
		| [] -> acc
		| h :: t ->
		(match h with
			| CtBool(s, e) -> constants_bmc a t (acc ^ sprintf "(define-const %s Bool %s)\n" s (expr_smt a "" false e))
			| CtInt(s, e) -> constants_bmc a t (acc ^ sprintf "(define-const %s Int %s)\n" s (expr_smt a "" false e)))
			
let rec inputs_bmc i n acc =
	match i with
		| [] -> acc
		| ProcIn(ty, IdInit(DDeclarator(st, []))) :: t -> inputs_bmc t n (acc ^ sprintf "(declare-fun %s ((_ BitVec %d)) %s)\n" st n (type_smt ty))
		| ProcIn(ty, IdName(DDeclarator(st, []))) :: t -> inputs_bmc t n (acc ^ sprintf "(declare-fun %s ((_ BitVec %d)) %s)\n" st n (type_smt ty))
		| ProcIn(ty, IdName(DDeclarator(st, [DsArray(l)]))) :: t -> inputs_bmc t n (acc ^ sprintf "(declare-fun %s ((_ BitVec %d)%s) %s)" st n (suffix_smt l "") (type_smt ty))
		| _ :: t -> inputs_bmc t n (acc ^ "NIY\n")
		
let rec declarations_bmc d b acc =
	match d with
		| [] -> acc
		| ProcDc(ty, DDeclarator(s, [])) :: t -> declarations_bmc t b (acc ^ sprintf "(declare-fun %s ((_ BitVec %d)) %s)\n" s b (type_smt ty))
		| _ :: t -> declarations_bmc t b (acc ^ "NIY\n")
			
let rec ranges_bmc a i d b acc =
	match i, d with
		| [], [] -> acc
		| ProcIn(TInt(IRanged(e1, e2)), IdName(DDeclarator(s, []))) :: t, d -> ranges_bmc a t d b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (and (<= (%s i) %s) (>= (%s i) %s))))\n" b s (expr_smt a "i" false e2) s (expr_smt a "i" false e1))
		| ProcIn(TInt(IRanged(e1, e2)), IdInit(DDeclarator(s, []))) :: t, d -> ranges_bmc a t d b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (and (<= (%s i) %s) (>= (%s i) %s))))\n" b s (expr_smt a "i" false e2) s (expr_smt a "i" false e1))
		| [], ProcDc(TInt(IRanged(e1, e2)), DDeclarator(s, [])) :: t -> ranges_bmc a [] t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (and (<= (%s i) %s) (>= (%s i) %s))))\n" b s (expr_smt a "i" false e2) s (expr_smt a "i" false e1))
		| [], _ :: t -> ranges_bmc a [] t b acc
		| _ :: t, _ -> ranges_bmc a t d b acc

let rec unroll_coll a rl acc =
	match rl with
		| [] -> acc
		| [x] ->
		(match x with
			| RExpr(e) -> acc ^ expr_smt a "i" false e
			| _ -> acc ^ "NIY\n")
		| h :: t ->
		(match h with
			| RExpr(e) -> unroll_coll a t (acc ^ expr_smt a "i" false e ^ " ")
			| _ -> unroll_coll a t (acc ^ "NIY\n"))

let rec definitions_bmc a d b acc =
	match d with
		| [] -> acc
		| h :: t ->
		(match h with
			| ProcDfAssign(_, LUnfolding([UlId(s)]), RExpr(e)) -> definitions_bmc a t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (= (%s i) %s)))\n" b s (expr_smt a "i" false e)) (*ça*)
			| ProcDfAssign(_, LUnfolding([UlId(s)]), RCollection(rl)) -> definitions_bmc a t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (= (%s i) (mk-str%d %s))))\n" b s (length rl) (unroll_coll a rl ""))
			| ProcDfInit(_, LUnfolding([UlId(s)]), RExpr(e)) -> definitions_bmc a t b (acc ^ sprintf "(assert (= (%s #x0) %s))\n" s (expr_smt a "i" false e))
			| ProcDfMemory(_, LUnfolding([UlId(s)]), RExpr(e1), RExpr(e2)) -> definitions_bmc a t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (= (%s i) (ite (= i #x0) %s %s))))\n" b s (expr_smt a "#x0" false e1) (expr_smt a "(bvadd i (bvneg #x1))" false e2)) (*ça*)
			| ProcDfNext(_, _, _) -> definitions_bmc a t b (acc ^ "NIY\n")
			| _ -> definitions_bmc a t b (acc ^ "NIY\n"))

let rec definitions_ind a d b acc =
	match d with
		| [] -> acc
		| h :: t ->
		(match h with
			| ProcDfAssign(_, LUnfolding([UlId(s)]), RExpr(e)) -> definitions_ind a t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (=> (bvult i %s) (= (%s (bvadd i #x1)) %s))))\n" b (_max_bv b "") s (expr_smt a "i" true e)) (*ça*)
			| ProcDfInit(_, _, _) -> definitions_ind a t b (acc ^ "NIY\n")
			| ProcDfMemory(_, LUnfolding([UlId(s)]), RExpr(_), RExpr(e2)) -> definitions_ind a t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (=> (bvult i %s) (= (%s (bvadd i #x1)) %s))))\n" b (_max_bv b "") s (expr_smt a "i" true e2)) (*ça*)
			| ProcDfNext(_, _, _) -> definitions_ind a t b (acc ^ "NIY\n")
			| _ -> definitions_ind a t b (acc ^ "NIY\n"))

let rec constraints_bmc ct cr b y acc =
	match cr with
		| [] -> acc
		| h :: t ->
		(match h with
			| CrCons(e) -> constraints_bmc ct t b y (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) (=> (bvult i #x%x) %s)))\n" b y (expr_smt ct "i" false e))
			| CrInit(_) -> constraints_bmc ct t b y (acc ^ "NIY\n"))

let rec constraints_ind ct cr b acc =
	match cr with
		| [] -> acc
		| h :: t ->
		(match h with
			| CrCons(e) -> constraints_ind ct t b (acc ^ sprintf "(assert (forall ((i (_ BitVec %d))) %s))\n" b (expr_smt ct "i" true e))
			| CrInit(_) -> constraints_ind ct t b (acc ^ "NIY\n"))

let proof_bmc c p d = sprintf "(assert (not %s))\n" (expr_smt c (sprintf "#x%x" d) false p)

let proof_ind c p df dl = sprintf "(assert (and %s (not %s)))\n" (expr_smt c (sprintf "#x%x" df) false p) (expr_smt c (sprintf "#x%x" dl) false p)

let generate_bmc_smt path l n a =
	match a with
	| {constants = []; constraints = []; declarations = []; definitions = []; inputs = []; namespaces = []; outputs = []; proofs = []; types = []} -> printf "[+] -smt : AST Invalid\n"
	| ast ->
		let b = bits_from_int l in
		for p = 1 to length ast.proofs do
			(if not (file_exists (sprintf "%s/po%d" path p) && is_directory (sprintf "%s/po%d" path p)) then mkdir (sprintf "%s/po%d" path p) 0o755;
			let solved = ref false in
			for i = 0 to l do
				let file = open_out (sprintf "%s/po%d/%d.smt" path p i) in
				let content =
					"(set-logic ALL)\n\n"
					^ "(define-fun _hll_min ((i1 Int) (i2 Int)) Int (ite (> i1 i2) i2 i1))\n"
					^ "(define-fun _hll_max ((i1 Int) (i2 Int)) Int (ite (> i1 i2) i1 i2))\n\n"
					^ "(declare-datatypes (T1 T2) ((str2 (mk-str2 (e1 T1) (e2 T2)))))\n\n"
					(* TYPES *)
					^ types_bmc ast ast.types "" ^ "\n"
					^ constants_bmc ast ast.constants "" ^ "\n"
					^ inputs_bmc ast.inputs b "" ^ "\n"
					^ declarations_bmc ast.declarations b "" ^ "\n"
					^ ranges_bmc ast ast.inputs ast.declarations b "" ^ "\n"
					^ definitions_bmc ast ast.definitions b "" ^ "\n"
					^ constraints_bmc ast ast.constraints b (l + n) "" ^ "\n"
					^ proof_bmc ast (nth ast.proofs (p - 1)) i ^ "\n"
					(* outputs / namespaces *)
					^ "(check-sat)\n(get-model)\n(exit)"
				in
				fprintf file "%s" content; close_out file;
				if not !solved then
					(let input = open_process_in (sprintf "z3 %s/po%d/%d.smt" path p i) in
					let res = input_line input in
					printf "[+] PO%d : z3 %s\t@%d\n" p res i;
					if res = "sat" then solved := true);
			done);
		done

let generate_ind_smt path l n a =
	match a with
	| {constants = []; constraints = []; declarations = []; definitions = []; inputs = []; namespaces = []; outputs = []; proofs = []; types = []} -> printf "[+] -smt : AST Invalid\n"
	| ast ->
		let b = bits_from_int l in
		for p = 1 to length ast.proofs do
			(if not (file_exists (sprintf "%s/po%d" path p) && is_directory (sprintf "%s/po%d" path p)) then mkdir (sprintf "%s/po%d" path p) 0o755;
				let file_base = open_out (sprintf "%s/po%d/base.smt" path p) in
				let file_step = open_out (sprintf "%s/po%d/step.smt" path p) in
				let content_base =
					"(set-logic ALL)\n\n"
					^ "(define-fun _hll_min ((i1 Int) (i2 Int)) Int (ite (> i1 i2) i2 i1))\n"
					^ "(define-fun _hll_max ((i1 Int) (i2 Int)) Int (ite (> i1 i2) i1 i2))\n\n"
					^ constants_bmc ast ast.constants "" ^ "\n"
					^ inputs_bmc ast.inputs b "" ^ "\n"
					^ declarations_bmc ast.declarations b "" ^ "\n"
					^ ranges_bmc ast ast.inputs ast.declarations b "" ^ "\n"
					^ definitions_bmc ast ast.definitions b "" ^ "\n"
					^ constraints_bmc ast ast.constraints b (l + n) "" ^ "\n"
					^ proof_bmc ast (_inv_not (nth ast.proofs (p - 1))) 0 ^ "\n"
					(* outputs / types / namespaces *)
					^ "(check-sat)\n(get-model)\n(exit)"
				in
				let content_step =
					"(set-logic ALL)\n\n"
					^ "(define-fun _hll_min ((i1 Int) (i2 Int)) Int (ite (> i1 i2) i2 i1))\n"
					^ "(define-fun _hll_max ((i1 Int) (i2 Int)) Int (ite (> i1 i2) i1 i2))\n\n"
					^ constants_bmc ast ast.constants "" ^ "\n"
					^ inputs_bmc ast.inputs b "" ^ "\n"
					^ declarations_bmc ast.declarations b "" ^ "\n"
					^ ranges_bmc ast ast.inputs ast.declarations b "" ^ "\n"
					^ definitions_ind ast ast.definitions b "" ^ "\n"
					^ constraints_ind ast ast.constraints b "" ^ "\n"
					^ proof_ind ast (nth ast.proofs (p - 1)) 0 1 ^ "\n"
					(* outputs / types / namespaces *)
					^ "(check-sat)\n(get-model)\n(exit)"
				in
				fprintf file_base "%s" content_base; close_out file_base;
				fprintf file_step "%s" content_step; close_out file_step;
				let input = open_process_in (sprintf "z3 %s/po%d/base.smt" path p) in
				let res = input_line input in
				printf "[+] PO%d : z3 base %s\n" p res;
				if res = "sat" then
					(let input = open_process_in (sprintf "z3 %s/po%d/step.smt" path p) in
					let res = input_line input in
					printf "[+] PO%d : z3 step %s\n" p res;));
		done