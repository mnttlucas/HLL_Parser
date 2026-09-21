open Alcotest
open Lib
open Lib.Ast

let pp (out:Format.formatter)(a:Ast.sections list option) = Format.pp_print_string out (Ast.ast_string a)
let tree = testable pp (fun x y -> x = y)

let test_constants = Some [
	SConstants([
		CtBool("t1", EClosed(CeTrue, []));
		CtBool("t2", EClosed(CeTrue, []));
		CtBool("t3", EClosed(CeTrue, []));
		CtBool("f1", EClosed(CeFalse, []));
		CtBool("f2", EClosed(CeFalse, []));
		CtBool("f3", EClosed(CeFalse, []));
		CtBool("e1", EIfThenElse(EClosed(CePath(PRelative(["f1"])), []), EClosed(CeTrue, []), [(EClosed(CePath(PRelative(["t2"])), []), EClosed(CeTrue, []))], EClosed(CeFalse, [])));
		CtBool("e2", EUnOp(UNot, EClosed(CePath(PRelative(["t1"])), [])));
		CtBool("e3", EBinOp(EClosed(CePath(PRelative(["t1"])), []), BAnd, EClosed(CePath(PRelative(["f1"])), [])));
		CtBool("e4", EBinOp(EClosed(CePath(PRelative(["f2"])), []), BOr, EClosed(CePath(PRelative(["f3"])), [])));
		CtBool("e5", EBinOp(EClosed(CePath(PRelative(["t2"])), []), BXor, EClosed(CePath(PRelative(["t3"])), [])));
		CtBool("e6", EBinOp(EClosed(CePath(PRelative(["e1"])), []), BImplication, EClosed(CePath(PRelative(["e4"])), [])));
		CtBool("e7", EBinOp(EClosed(CePath(PRelative(["e5"])), []), BEquivalence, EClosed(CePath(PRelative(["e6"])), [])))
	]);
	SConstants([
		CtInt("\'i1\'", EClosed(CeIntLit("0"), []));
		CtInt("\'i2\'", EClosed(CeIntLit("123456789"), []));
		CtInt("\'i3\'", EClosed(CeIntLit("0b01"), []));
		CtInt("\'i4\'", EClosed(CeIntLit("0B10"), []));
		CtInt("\'i5\'", EClosed(CeIntLit("0xFB50"), []));
		CtInt("\'i6\'", EClosed(CeIntLit("0XFB50"), []));
		CtInt("\"e1\"", EBinOp(EClosed(CePath(PRelative(["\'i1\'"])), []), BSum, EClosed(CePath(PRelative(["\'i3\'"])), [])));
		CtInt("\"e2\"", EBinOp(EClosed(CePath(PRelative(["\'i2\'"])), []), BProduct, EClosed(CePath(PRelative(["\'i4\'"])), [])));
		CtInt("\"e3\"", EBinOp(EClosed(CePath(PRelative(["\'i2\'"])), []), BDifference, EClosed(CePath(PRelative(["\'i6\'"])), [])));
		CtInt("\"e4\"", EBinOp(EClosed(CePath(PRelative(["\'i3\'"])), []), BDivision, EClosed(CePath(PRelative(["\'i4\'"])), [])));
		CtInt("\"e5\"", EUnOp(UMinus, EClosed(CePath(PRelative(["\"e1\""])), [])));
		CtInt("\"e6\"", EBinOp(EClosed(CePath(PRelative(["\'i2\'"])), []), BPower, EClosed(CePath(PRelative(["\'i3\'"])), [])));
		CtInt("\"e7\"", EBinOp(EClosed(CePath(PRelative(["\'i3\'"])), []), BModulo, EClosed(CePath(PRelative(["\'i4\'"])), [])));
		CtInt("\"e8\"", EBinOp(EClosed(CePath(PRelative(["\'i3\'"])), []), BFloor, EClosed(CePath(PRelative(["\'i4\'"])), [])));
		CtInt("\"e9\"", EBinOp(EClosed(CePath(PRelative(["\'i3\'"])), []), BCeil, EClosed(CePath(PRelative(["\'i4\'"])), [])));
		CtInt("\"e10\"", EBinOp(EClosed(CePath(PRelative(["\'i5\'"])), []), BShift_Right, EClosed(CePath(PRelative(["\'i3\'"])), [])));
		CtInt("\"e11\"", EBinOp(EClosed(CePath(PRelative(["\'i5\'"])), []), BShift_Left, EClosed(CePath(PRelative(["\'i3\'"])), [])))
	]);
	SConstants([
		CtBool("c1", EBinOp(EClosed(CePath(PRelative(["\'i1\'"])), []), BGreater, EClosed(CePath(PRelative(["\'i6\'"])), [])));
		CtBool("c2", EBinOp(EClosed(CePath(PRelative(["\'i5\'"])), []), BGreater_Eq, EClosed(CePath(PRelative(["\'i2\'"])), [])));
		CtBool("c3", EBinOp(EClosed(CePath(PRelative(["\'i1\'"])), []), BLess, EClosed(CePath(PRelative(["\'i6\'"])), [])));
		CtBool("c4", EBinOp(EClosed(CePath(PRelative(["\'i2\'"])), []), BLess_Eq, EClosed(CePath(PRelative(["\'i3\'"])), [])));
		CtBool("c5", EBinOp(EClosed(CePath(PRelative(["c1"])), []), BEqual, EClosed(CePath(PRelative(["c2"])), [])));
		CtBool("c6", EBinOp(EClosed(CePath(PRelative(["\'i3\'"])), []), BEqual, EClosed(CePath(PRelative(["\'i4\'"])), [])));
		CtBool("c7", EBinOp(EClosed(CePath(PRelative(["\'i5\'"])), []), BDifferent, EClosed(CePath(PRelative(["\'i6\'"])), [])));
		CtBool("c8", EBinOp(EClosed(CePath(PRelative(["t3"])), []), BDifferent, EClosed(CePath(PRelative(["f3"])), [])));
	])]
let test_constraints = Some [
	SConstraints([
		CrInit(EClosed(CePath(PRelative(["s1"])), []));
		CrCons(EBinOp(EClosed(CePath(PRelative(["s2"])), [AFunction([EClosed(CeIntLit("0"), [])])]), BEqual, EUnOp(UMinus, EClosed(CeIntLit("5"), []))))
	]);
	SConstraints([
		CrCons(EBinOp(EClosed(CePath(PRelative(["s3"])), []), BOr, EClosed(CePath(PRelative(["s4"])), [])))
	])]
let test_declarations = Some [
	SDeclarations([
		DcTyped(TInt(IInt), [DDeclarator("f", [DsFunction([TInt(IInt)]); DsFunction([TInt(IInt)])]); DDeclarator("arr", [DsArray([EClosed(CeIntLit("3"), []); EClosed(CeIntLit("3"), [])]); DsFunction([TInt(IInt)])])]);
		DcTyped(TBool, [DDeclarator("b", []); DDeclarator("m5", [DsFunction([TBool; TBool; TBool; TBool; TBool])])])
	]);
	SDeclarations([
		DcUntyped([DDeclarator("d1", []); DDeclarator("d2", [])])
	])]
let test_definitions = Some [
	SDefinitions([
		DfAssign(LUnfolding([UlId("f1"); UlId("f2"); UlUnderscore]), RExpr(EClosed(CeIntLit("0"), [])));
		DfAssign(LUnfolding([UlId("t")]), RCollection([RExpr(EClosed(CeIntLit("1"), [])); RExpr(EClosed(CeTrue, [])); RExpr(EClosed(CeIntLit("0xC0"), []))]));
		DfAssign(LParams("arr", [FpArray(["i1"; "i2"])]), RExpr(EClosed(CeIntLit("0"), [])));
		DfAssign(LParams("f3", [FpFunction(["p1"]); FpFunction(["p2"])]), RExpr(EUnOp(UMinus, EClosed(CeIntLit("1"), []))));
		DfAssign(LUnfolding([UlId("comp")]), RCollection([RExpr(EClosed(CeTrue, [])); RCollection([RExpr(EClosed(CeIntLit("0"), [])); RCollection([RExpr(EClosed(CeFalse, [])); RExpr(EClosed(CeIntLit("2"), []))])])]));
		DfAssign(LUnfolding([UlUnderscore]), RExpr(EClosed(CeFalse, [])))
	]);
	SDefinitions([
		DfInit(LUnfolding([UlId("s1")]), RExpr(EClosed(CeTrue, [])));
		DfNext(LUnfolding([UlId("s1")]), RExpr(EClosed(CeFalse, [])))
	]);
	SDefinitions([
		DfMemory(LUnfolding([UlId("s3")]), RExpr(EClosed(CeTrue, [])), RExpr(EClosed(CePath(PRelative(["s2"])), [])));
		DfAssign(LUnfolding([UlId("l")]), RExpr(ELambda([DsArray([EClosed(CeIntLit("3"), [])])], [FpArray(["i"])], EBinOp(EClosed(CePath(PRelative(["i"])), []), BEqual, EClosed(CeIntLit("1"), [])))));
		DfAssign(LUnfolding([UlId("fibonacci")]), RExpr(ELambda([DsFunction([TInt(IInt)])], [FpFunction(["i"])], EIfThenElse(EBinOp(EClosed(CePath(PRelative(["i"])), []), BLess_Eq, EClosed(CeIntLit("2"), [])), EClosed(CeIntLit("1"), []) , [], EBinOp(EClosed(CePath(PRelative(["fibonacci"])), [AFunction([EBinOp(EClosed(CePath(PRelative(["i"])), []), BDifference, EClosed(CeIntLit("1"), []))])]), BSum, EClosed(CePath(PRelative(["fibonacci"])), [AFunction([EBinOp(EClosed(CePath(PRelative(["i"])), []), BDifference, EClosed(CeIntLit("2"), []))])]))))));
		DfAssign(LUnfolding([UlId("m1")]), RExpr(EMember(EClosed(CePath(PRelative(["s3"])), []), DBool)));
		DfAssign(LUnfolding([UlId("m2")]), RExpr(EMember(EClosed(CePath(PRelative(["s1"])), []), DInt)));
		DfAssign(LUnfolding([UlId("m3")]), RExpr(EMember(EClosed(CePath(PRelative(["func"])), []), DPath(PAbsolute(["ns"; "val"])))));
		DfAssign(LUnfolding([UlId("m4")]), RExpr(EMember(EClosed(CePath(PRelative(["i2"])), []), DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("100"), [])))));
		DfAssign(LUnfolding([UlId("ne")]), RExpr(EClosed(CePath(PRelative(["ns"; "expr"])), [])));
		DfAssign(LUnfolding([UlId("n")]), RExpr(EClosed(CeNext(EClosed(CePath(PRelative(["ns"])), [])), [])));
		DfAssign(LUnfolding([UlId("pr1")]), RExpr(EClosed(CeTypedPre2(TBool, EClosed(CePath(PRelative(["s1"])), []), EClosed(CeTrue, [])), [])));
		DfAssign(LUnfolding([UlId("pr2")]), RExpr(EClosed(CeTypedPre1(TInt(IInt), EClosed(CePath(PRelative(["f1"])), [])), [])));
		DfAssign(LUnfolding([UlId("pr3")]), RExpr(EClosed(CeUntypedPre1(EClosed(CePath(PRelative(["s3"])), [])), [])));
		DfAssign(LUnfolding([UlId("pr4")]), RExpr(EClosed(CeUntypedPre2(EClosed(CePath(PRelative(["pr3"])), []), EClosed(CeFalse, [])), [])));
		DfAssign(LUnfolding([UlId("f1")]), RExpr(EClosed(CeFop(FDmin, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f2")]), RExpr(EClosed(CeFop(FDmax, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f3")]), RExpr(EClosed(CeFop(FDabs, [EClosed(CePath(PRelative(["n"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f4")]), RExpr(EClosed(CeFop(FDor, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s3"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f5")]), RExpr(EClosed(CeFop(FDand, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f6")]), RExpr(EClosed(CeFop(FDxor, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f7")]), RExpr(EClosed(CeFop(FDnot, [EClosed(CePath(PRelative(["s1"])), [])]), [])));
		DfAssign(LUnfolding([UlId("f8")]), RExpr(EClosed(CeFop(Fb2u, [EClosed(CePath(PRelative(["b"])), []); EClosed(CeIntLit("3"), [])]), [])));
		DfAssign(LUnfolding([UlId("f9")]), RExpr(EClosed(CeFop(Fu2b, [EClosed(CePath(PRelative(["i"])), []); EClosed(CeIntLit("3"), [])]), [])));
		DfAssign(LUnfolding([UlId("f10")]), RExpr(EClosed(CeFop(Fb2s, [EClosed(CePath(PRelative(["b"])), []);  EClosed(CeIntLit("3"), [])]), [])));
		DfAssign(LUnfolding([UlId("f11")]), RExpr(EClosed(CeFop(Fs2b, [EClosed(CePath(PRelative(["i"])), []);  EClosed(CeIntLit("3"), [])]), [])));
		DfAssign(LUnfolding([UlId("f12")]), RExpr(EClosed(CeFop(FPceq, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), []); EClosed(CePath(PRelative(["s3"])), []); EClosed(CeIntLit("0"), [])]), [])));
		DfAssign(LUnfolding([UlId("f13")]), RExpr(EClosed(CeFop(FPcgt, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), []); EClosed(CePath(PRelative(["s3"])), []); EClosed(CeIntLit("0"), [])]), [])));
		DfAssign(LUnfolding([UlId("f14")]), RExpr(EClosed(CeFop(FPclt, [EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), []); EClosed(CePath(PRelative(["s3"])), []); EClosed(CeIntLit("0"), [])]), [])));
		DfAssign(LUnfolding([UlId("c")]), RExpr(EClosed(CeCast(TInt(ISigned(OInt("16"))), EClosed(CePath(PRelative(["f2"])), [])), [])));
		DfAssign(LUnfolding([UlId("w")]), RExpr(EClosed(CeWith(EClosed(CePath(PRelative(["str"])), []), [AStruct("el1")], RExpr(EClosed(CeTrue, []))), [])));
		DfAssign(LUnfolding([UlId("sw")]), RExpr(EClosed(CeSwitch([EClosed(CePath(PRelative(["s1"])), []); EClosed(CePath(PRelative(["s2"])), []); EClosed(CePath(PRelative(["s3"])), [])], [([PExpr(EClosed(CeTrue, [])); PExpr(EClosed(CeTrue, [])); PExpr(EClosed(CeTrue, []))], EClosed(CeIntLit("0"), [])); ([PExpr(EClosed(CeFalse, [])); PExpr(EClosed(CeFalse, [])); PExpr(EClosed(CeFalse, []))], EClosed(CeIntLit("9"), [])); ([PUnderscore; PPath(PRelative(["ns"; "t"]), "nst"); PPathUnderscore(PRelative(["ns"; "t"]))], EClosed(CeIntLit("3"), []))]), [])));
		DfAssign(LUnfolding([UlId("q1")]), RExpr(EClosed(CeQuantif(QeExpr(QAll, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EBinOp(EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]), BEqual, EClosed(CeIntLit("0"), [])))), [])));
		DfAssign(LUnfolding([UlId("q2")]), RExpr(EClosed(CeQuantif(QeExpr(QSome, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EBinOp(EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]), BEqual, EClosed(CeIntLit("0"), [])))), [])));
		DfAssign(LUnfolding([UlId("q3")]), RExpr(EClosed(CeQuantif(QeExpr(QSum, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]))), [])));
		DfAssign(LUnfolding([UlId("q4")]), RExpr(EClosed(CeQuantif(QeExpr(QProd, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]))), [])));
		DfAssign(LUnfolding([UlId("q5")]), RExpr(EClosed(CeQuantif(QeExpr(QDmin, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]))), [])));
		DfAssign(LUnfolding([UlId("q6")]), RExpr(EClosed(CeQuantif(QeExpr(QDmax, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]))), [])));
		DfAssign(LUnfolding([UlId("q7")]), RExpr(EClosed(CeQuantif(QeExpr(QConj, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EBinOp(EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]), BEqual, EClosed(CeIntLit("0"), [])))), [])));
		DfAssign(LUnfolding([UlId("q8")]), RExpr(EClosed(CeQuantif(QeExpr(QDisj, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EBinOp(EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]), BEqual, EClosed(CeIntLit("0"), [])))), [])));
		DfAssign(LUnfolding([UlId("q9")]), RExpr(EClosed(CeQuantif(QeQuantif(QAll, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("100"), [])))], QeExpr(QAll, [QvDomain("j", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("100"), [])))], EBinOp(EClosed(CePath(PRelative(["P"])), [AFunction([EClosed(CePath(PRelative(["i"])), [])])]), BImplication, EClosed(CePath(PRelative(["P"])), [AFunction([EClosed(CePath(PRelative(["j"])), [])])]))))), [])));
		DfAssign(LUnfolding([UlId("q10")]), RExpr(EClosed(CeQuantif(QeExpr(QAll, [QvItems("a", EClosed(CePath(PRelative(["A"])), []))], EBinOp(EClosed(CePath(PRelative(["a"])), []), BEqual, EClosed(CeIntLit("0"), [])))), [])));
		DfAssign(LUnfolding([UlId("q11")]), RExpr(EClosed(CeQuantif(QeExpr(QSelect, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EBinOp(EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]), BEqual, EClosed(CeIntLit("0"), [])))), [])));
		DfAssign(LUnfolding([UlId("q12")]), RExpr(EClosed(CeQuantif(QeExprRhs(QSelect, [QvDomain("i", DRange(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("2"), [])))], EBinOp(EClosed(CePath(PRelative(["A"])), [AArray([EClosed(CePath(PRelative(["i"])), [])])]), BEqual, EClosed(CeIntLit("0"), [])), RExpr(EClosed(CeIntLit("0"), [])))), [])))
	])]
let test_inputs = Some [
	SInputs([
		ITyped(TBool, [IdName(DDeclarator("b", [DsArray([EClosed(CeIntLit("1"), []); EClosed(CeIntLit("2"), [])])])); IdInit(DDeclarator("k", [DsArray([EClosed(CeIntLit("0xFF"), []); EUnOp(UMinus, EClosed(CeIntLit("5"), []))])]))]);
		ITyped(TInt(IInt), [IdName(DDeclarator("f", [DsFunction([TInt(IInt); TInt(IInt)])])); IdInit(DDeclarator("in", [])); IdInit(DDeclarator("j", [DsFunction([TBool; TInt(IInt)])]))])
	]);
	SInputs([
		IUntyped([IdName(DDeclarator("u1", [])); IdInit(DDeclarator("u2", []))])
	])]
let test_namespaces = Some [
	SNamespaces([
		"ns1",
		[
		SConstants([
			CtInt("i", EClosed(CeIntLit("16"), []))
		]);
		STypes([
			TType(TBool, [DDeclarator("b1", [])]);
			TType(TBool, [DDeclarator("b2", []); DDeclarator("b3", [])]);
			TType(TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("100"), []))), [DDeclarator("i1", []); DDeclarator("i2", []); DDeclarator("i3", [])]);
			TType(TInt(IUnsigned(OInt("32"))), [DDeclarator("i4", [])]);
			TType(TInt(ISigned(OInt("8"))), [DDeclarator("i5", []); DDeclarator("i6", [])]);
			TType(TInt(ISigned(OId("i"))), [DDeclarator("i7", [])]);
		]);
		STypes([
			TType(TTuple([TBool; TInt(IInt); TInt(IUnsigned(OInt("8")))]), [DDeclarator("t", [])]);
			TType(TStructure([("s", TBool); ("v", TInt(ISigned(OInt("16")))); ("m", TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("360"), []))))]), [DDeclarator("st", [])]);
			TType(TArray(TInt(IInt), [EClosed(CePath(PRelative(["i"])), [])]), [DDeclarator("arr", [])]);
			TType(TArray(TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("9"), []))), [EClosed(CeIntLit("3"), []); EClosed(CeIntLit("3"), [])]), [DDeclarator("grid", [])]);
			TType(TFunction([TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("360"), []))); TInt(IInt)], TInt(IInt)), [DDeclarator("conv1", [])]);
			TType(TFunction([TInt(IInt)], TInt(IInt)), [DDeclarator("conv2", [])]);
			TType(TPath(PRelative(["ns1"; "id1"])), [DDeclarator("nt1", [])]);
			TType(TPath(PAbsolute(["ns2"; "id2"])), [DDeclarator("nt2", [])]);
			TSort("s");
			TSortContrib(ScPaths([PRelative(["s"])]), "r");
			TSortContrib(ScIds(["s1"; "s2"; "s3"]), "r");
			TSortContrib(ScPaths([PRelative(["ns3"; "ns4"; "id3"]); PAbsolute(["ns5"; "ns6"; "id4"])]), "s4");
			TEnum(["e1"; "e2"; "e3"], "en1");
			TEnum(["e4"], "en2")
		])]
	]);
	SNamespaces([
		"ns2",
		[
		SConstants([
			CtBool("t1", EClosed(CeTrue, []));
			CtBool("t2", EClosed(CeTrue, []));
			CtBool("t3", EClosed(CeTrue, []));
			CtBool("f1", EClosed(CeFalse, []));
			CtBool("f2", EClosed(CeFalse, []));
			CtBool("f3", EClosed(CeFalse, []));
			CtBool("e1", EIfThenElse(EClosed(CePath(PRelative(["f1"])), []), EClosed(CeTrue, []), [(EClosed(CePath(PRelative(["t2"])), []), EClosed(CeTrue, []))], EClosed(CeFalse, [])));
			CtBool("e2", EUnOp(UNot, EClosed(CePath(PRelative(["t1"])), [])));
			CtBool("e3", EBinOp(EClosed(CePath(PRelative(["t1"])), []), BAnd, EClosed(CePath(PRelative(["f1"])), [])));
			CtBool("e4", EBinOp(EClosed(CePath(PRelative(["f2"])), []), BOr, EClosed(CePath(PRelative(["f3"])), [])));
			CtBool("e5", EBinOp(EClosed(CePath(PRelative(["t2"])), []), BXor, EClosed(CePath(PRelative(["t3"])), [])));
			CtBool("e6", EBinOp(EClosed(CePath(PRelative(["e1"])), []), BImplication, EClosed(CePath(PRelative(["e4"])), [])));
			CtBool("e7", EBinOp(EClosed(CePath(PRelative(["e5"])), []), BEquivalence, EClosed(CePath(PRelative(["e6"])), [])))
		])]
	])]
let test_outputs = Some [
	SOutputs([
		EClosed(CePath(PRelative(["o1"])), []);
		EClosed(CePath(PRelative(["o2"])), [])
	]);
	SOutputs([
		EClosed(CePath(PRelative(["o3"])), [AArray([EClosed(CeIntLit("1"), [])])]);
		EClosed(CePath(PRelative(["o4"])), [AStruct("a")]);
		EUnOp(UNot, EClosed(CePath(PRelative(["o5"])), []));
		EClosed(CePath(PRelative(["o6"])), [ATuple("0")]);
		EClosed(CePath(PRelative(["o7"])), [AFunction([EClosed(CeIntLit("2"), [])])])
	])]
let test_proofs = Some [
	SProofs([
		EClosed(CePath(PRelative(["po1"])), [])
	]);
	SProofs([
		EClosed(CeExpr((EClosed(CePath(PRelative(["po2"])), [AArray([EClosed(CeIntLit("0"), [])])]))), [])
	]);
	SProofs([
		EBinOp(EBinOp(EClosed(CePath(PRelative(["po3"])), []), BAnd, EClosed(CePath(PRelative(["po4"])), [])), BAnd, EUnOp(UNot, EClosed(CePath(PRelative(["po5"])), [])))
	]);
	SProofs([
		EBinOp(EClosed(CePath(PRelative(["po6"])), []), BDifferent, EClosed(CeIntLit("0"), []))
	]);]
let test_types = Some [
	SConstants([
		CtInt("i", EClosed(CeIntLit("16"), []))
	]);
	STypes([
		TType(TBool, [DDeclarator("b1", [])]);
		TType(TBool, [DDeclarator("b2", []); DDeclarator("b3", [])]);
		TType(TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("100"), []))), [DDeclarator("i1", []); DDeclarator("i2", []); DDeclarator("i3", [])]);
		TType(TInt(IUnsigned(OInt("32"))), [DDeclarator("i4", [])]);
		TType(TInt(ISigned(OInt("8"))), [DDeclarator("i5", []); DDeclarator("i6", [])]);
		TType(TInt(ISigned(OId("i"))), [DDeclarator("i7", [])]);
	]);
	STypes([
		TType(TTuple([TBool; TInt(IInt); TInt(IUnsigned(OInt("8")))]), [DDeclarator("t", [])]);
		TType(TStructure([("s", TBool); ("v", TInt(ISigned(OInt("16")))); ("m", TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("360"), []))))]), [DDeclarator("st", [])]);
		TType(TArray(TInt(IInt), [EClosed(CePath(PRelative(["i"])), [])]), [DDeclarator("arr", [])]);
		TType(TArray(TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("9"), []))), [EClosed(CeIntLit("3"), []); EClosed(CeIntLit("3"), [])]), [DDeclarator("grid", [])]);
		TType(TFunction([TInt(IRanged(EClosed(CeIntLit("0"), []), EClosed(CeIntLit("360"), []))); TInt(IInt)], TInt(IInt)), [DDeclarator("conv1", [])]);
		TType(TFunction([TInt(IInt)], TInt(IInt)), [DDeclarator("conv2", [])]);
		TType(TPath(PRelative(["ns1"; "id1"])), [DDeclarator("nt1", [])]);
		TType(TPath(PAbsolute(["ns2"; "id2"])), [DDeclarator("nt2", [])]);
		TSort("s");
		TSortContrib(ScPaths([PRelative(["s"])]), "r");
		TSortContrib(ScIds(["s1"; "s2"; "s3"]), "r");
		TSortContrib(ScPaths([PRelative(["ns3"; "ns4"; "id3"]); PAbsolute(["ns5"; "ns6"; "id4"])]), "s4");
		TEnum(["e1"; "e2"; "e3"], "en1");
		TEnum(["e4"], "en2")
	])]

let test_0 () = 
	Alcotest.(check tree) "same" test_constants (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_constants.hll")))

let test_1 () = 
	Alcotest.(check tree) "same" test_constraints (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_constraints.hll")))

let test_2 () = 
	Alcotest.(check tree) "same" test_declarations (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_declarations.hll")))

let test_3 () = 
	Alcotest.(check tree) "same" test_definitions (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_definitions.hll")))

let test_4 () = 
	Alcotest.(check tree) "same" test_inputs (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_inputs.hll")))

let test_5 () = 
	Alcotest.(check tree) "same" test_namespaces (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_namespaces.hll")))

let test_6 () = 
	Alcotest.(check tree) "same" test_outputs (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_outputs.hll")))

let test_7 () = 
	Alcotest.(check tree) "same" test_proofs (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_proofs.hll")))

let test_8 () = 
	Alcotest.(check tree) "same" test_types (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_types.hll")))

let tests = [
	test_case "Constants Test"			`Quick test_0;
	test_case "Constraints Test"		`Quick test_1;
	test_case "Declarations Test"		`Quick test_2;
	test_case "Definitions Test"		`Quick test_3;
	test_case "Inputs Test"				`Quick test_4;
	test_case "Namespaces Test"			`Quick test_5;
	test_case "Outputs Test"			`Quick test_6;
	test_case "Proof Obligations Test"	`Quick test_7;
	test_case "Types Test"				`Quick test_8;
]