type _String = string
type 'a _List = 'a list
type 'a _Maybe = Nothing | Just of 'a
type _Bool = True | False
type _Int = int
type _Integer
type _Char
type _Rational

(* generated part *)
type ('a) _ParseResult = 
  | ParseOk of 'a
  | ParseFailed of _SrcLoc * _String
and _Module = 
  | Module of _SrcLoc * _ModuleName * (_ModulePragma)_List * ((_WarningText)_Maybe) * (((_ExportSpec)_List)_Maybe) * (_ImportDecl)_List * (_Decl)_List
and _WarningText = 
  | DeprText of _String
  | WarnText of _String
and _ExportSpec = 
  | EVar of _QName
  | EAbs of _QName
  | EThingAll of _QName
  | EThingWith of _QName * (_CName)_List
  | EModuleContents of _ModuleName
and _ImportDecl = {importLoc : _SrcLoc ; importModule : _ModuleName ; importQualified : _Bool ; importSrc : _Bool ; importPkg : (_String)_Maybe ; importAs : (_ModuleName)_Maybe ; importSpecs : (_Bool * (_ImportSpec)_List)_Maybe}
and _ImportSpec = 
  | IVar of _Name
  | IAbs of _Name
  | IThingAll of _Name
  | IThingWith of _Name * (_CName)_List
and _Assoc = 
  | AssocNone
  | AssocLeft
  | AssocRight
and _Decl = 
  | TypeDecl of _SrcLoc * _Name * (_TyVarBind)_List * _Type
  | TypeFamDecl of _SrcLoc * _Name * (_TyVarBind)_List * ((_Kind)_Maybe)
  | DataDecl of _SrcLoc * _DataOrNew * _Context * _Name * (_TyVarBind)_List * (_QualConDecl)_List * (_Deriving)_List
  | GDataDecl of _SrcLoc * _DataOrNew * _Context * _Name * (_TyVarBind)_List * ((_Kind)_Maybe) * (_GadtDecl)_List * (_Deriving)_List
  | DataFamDecl of _SrcLoc * _Context * _Name * (_TyVarBind)_List * ((_Kind)_Maybe)
  | TypeInsDecl of _SrcLoc * _Type * _Type
  | DataInsDecl of _SrcLoc * _DataOrNew * _Type * (_QualConDecl)_List * (_Deriving)_List
  | GDataInsDecl of _SrcLoc * _DataOrNew * _Type * ((_Kind)_Maybe) * (_GadtDecl)_List * (_Deriving)_List
  | ClassDecl of _SrcLoc * _Context * _Name * (_TyVarBind)_List * (_FunDep)_List * (_ClassDecl)_List
  | InstDecl of _SrcLoc * _Context * _QName * (_Type)_List * (_InstDecl)_List
  | DerivDecl of _SrcLoc * _Context * _QName * (_Type)_List
  | InfixDecl of _SrcLoc * _Assoc * _Int * (_Op)_List
  | DefaultDecl of _SrcLoc * (_Type)_List
  | SpliceDecl of _SrcLoc * _Exp
  | TypeSig of _SrcLoc * (_Name)_List * _Type
  | FunBind of (_Match)_List
  | PatBind of _SrcLoc * _Pat * ((_Type)_Maybe) * _Rhs * _Binds
  | ForImp of _SrcLoc * _CallConv * _Safety * _String * _Name * _Type
  | ForExp of _SrcLoc * _CallConv * _String * _Name * _Type
  | RulePragmaDecl of _SrcLoc * (_Rule)_List
  | DeprPragmaDecl of _SrcLoc * ((_Name)_List * _String)_List
  | WarnPragmaDecl of _SrcLoc * ((_Name)_List * _String)_List
  | InlineSig of _SrcLoc * _Bool * _Activation * _QName
  | InlineConlikeSig of _SrcLoc * _Activation * _QName
  | SpecSig of _SrcLoc * _QName * (_Type)_List
  | SpecInlineSig of _SrcLoc * _Bool * _Activation * _QName * (_Type)_List
  | InstSig of _SrcLoc * _Context * _QName * (_Type)_List
  | AnnPragma of _SrcLoc * _Annotation
and _Binds = 
  | BDecls of (_Decl)_List
  | IPBinds of (_IPBind)_List
and _IPBind = 
  | IPBind of _SrcLoc * _IPName * _Exp
and _ClassDecl = 
  | ClsDecl of _Decl
  | ClsDataFam of _SrcLoc * _Context * _Name * (_TyVarBind)_List * ((_Kind)_Maybe)
  | ClsTyFam of _SrcLoc * _Name * (_TyVarBind)_List * ((_Kind)_Maybe)
  | ClsTyDef of _SrcLoc * _Type * _Type
and _InstDecl = 
  | InsDecl of _Decl
  | InsType of _SrcLoc * _Type * _Type
  | InsData of _SrcLoc * _DataOrNew * _Type * (_QualConDecl)_List * (_Deriving)_List
  | InsGData of _SrcLoc * _DataOrNew * _Type * ((_Kind)_Maybe) * (_GadtDecl)_List * (_Deriving)_List
and _Deriving = _QName * (_Type)_List
and _DataOrNew = 
  | DataType
  | NewType
and _ConDecl = 
  | ConDecl of _Name * (_BangType)_List
  | InfixConDecl of _BangType * _Name * _BangType
  | RecDecl of _Name * ((_Name)_List * _BangType)_List
and _QualConDecl = 
  | QualConDecl of _SrcLoc * (_TyVarBind)_List * _Context * _ConDecl
and _GadtDecl = 
  | GadtDecl of _SrcLoc * _Name * _Type
and _BangType = 
  | BangedTy of _Type
  | UnBangedTy of _Type
  | UnpackedTy of _Type
and _Match = 
  | Match of _SrcLoc * _Name * (_Pat)_List * ((_Type)_Maybe) * _Rhs * _Binds
and _Rhs = 
  | UnGuardedRhs of _Exp
  | GuardedRhss of (_GuardedRhs)_List
and _GuardedRhs = 
  | GuardedRhs of _SrcLoc * (_Stmt)_List * _Exp
and _Context = (_Asst)_List
and _FunDep = 
  | FunDep of (_Name)_List * (_Name)_List
and _Asst = 
  | ClassA of _QName * (_Type)_List
  | InfixA of _Type * _QName * _Type
  | IParam of _IPName * _Type
  | EqualP of _Type * _Type
and _Type = 
  | TyForall of (((_TyVarBind)_List)_Maybe) * _Context * _Type
  | TyFun of _Type * _Type
  | TyTuple of _Boxed * (_Type)_List
  | TyList of _Type
  | TyApp of _Type * _Type
  | TyVar of _Name
  | TyCon of _QName
  | TyParen of _Type
  | TyInfix of _Type * _QName * _Type
  | TyKind of _Type * _Kind
and _Boxed = 
  | Boxed
  | Unboxed
and _Kind = 
  | KindStar
  | KindBang
  | KindFn of _Kind * _Kind
  | KindParen of _Kind
  | KindVar of _Name
and _TyVarBind = 
  | KindedVar of _Name * _Kind
  | UnkindedVar of _Name
and _Exp = 
  | Var of _QName
  | IPVar of _IPName
  | Con of _QName
  | Lit of _Literal
  | InfixApp of _Exp * _QOp * _Exp
  | App of _Exp * _Exp
  | NegApp of _Exp
  | Lambda of _SrcLoc * (_Pat)_List * _Exp
  | Let of _Binds * _Exp
  | If of _Exp * _Exp * _Exp
  | Case of _Exp * (_Alt)_List
  | Do of (_Stmt)_List
  | MDo of (_Stmt)_List
  | Tuple of (_Exp)_List
  | TupleSection of ((_Exp)_Maybe)_List
  | List of (_Exp)_List
  | Paren of _Exp
  | LeftSection of _Exp * _QOp
  | RightSection of _QOp * _Exp
  | RecConstr of _QName * (_FieldUpdate)_List
  | RecUpdate of _Exp * (_FieldUpdate)_List
  | EnumFrom of _Exp
  | EnumFromTo of _Exp * _Exp
  | EnumFromThen of _Exp * _Exp
  | EnumFromThenTo of _Exp * _Exp * _Exp
  | ListComp of _Exp * (_QualStmt)_List
  | ParComp of _Exp * ((_QualStmt)_List)_List
  | ExpTypeSig of _SrcLoc * _Exp * _Type
  | VarQuote of _QName
  | TypQuote of _QName
  | BracketExp of _Bracket
  | SpliceExp of _Splice
  | QuasiQuote of _String * _String
  | XTag of _SrcLoc * _XName * (_XAttr)_List * ((_Exp)_Maybe) * (_Exp)_List
  | XETag of _SrcLoc * _XName * (_XAttr)_List * ((_Exp)_Maybe)
  | XPcdata of _String
  | XExpTag of _Exp
  | XChildTag of _SrcLoc * (_Exp)_List
  | CorePragma of _String * _Exp
  | SCCPragma of _String * _Exp(* 2do *)
  | Proc of _SrcLoc * _Pat * _Exp
  | LeftArrApp of _Exp * _Exp
  | RightArrApp of _Exp * _Exp
  | LeftArrHighApp of _Exp * _Exp
  | RightArrHighApp of _Exp * _Exp
and _Stmt = 
  | Generator of _SrcLoc * _Pat * _Exp
  | Qualifier of _Exp
  | LetStmt of _Binds
  | RecStmt of (_Stmt)_List
and _QualStmt = 
  | QualStmt of _Stmt
  | ThenTrans of _Exp
  | ThenBy of _Exp * _Exp
  | GroupBy of _Exp
  | GroupUsing of _Exp
  | GroupByUsing of _Exp * _Exp
and _FieldUpdate = 
  | FieldUpdate of _QName * _Exp
  | FieldPun of _Name
  | FieldWildcard
and _Alt = 
  | Alt of _SrcLoc * _Pat * _GuardedAlts * _Binds
and _GuardedAlts = 
  | UnGuardedAlt of _Exp
  | GuardedAlts of (_GuardedAlt)_List
and _GuardedAlt = 
  | GuardedAlt of _SrcLoc * (_Stmt)_List * _Exp
and _XAttr = 
  | XAttr of _XName * _Exp
and _Pat = 
  | PVar of _Name
  | PLit of _Literal
  | PNeg of _Pat
  | PNPlusK of _Name * _Integer
  | PInfixApp of _Pat * _QName * _Pat
  | PApp of _QName * (_Pat)_List
  | PTuple of (_Pat)_List
  | PList of (_Pat)_List
  | PParen of _Pat
  | PRec of _QName * (_PatField)_List
  | PAsPat of _Name * _Pat
  | PWildCard
  | PIrrPat of _Pat
  | PatTypeSig of _SrcLoc * _Pat * _Type
  | PViewPat of _Exp * _Pat
  | PRPat of (_RPat)_List
  | PXTag of _SrcLoc * _XName * (_PXAttr)_List * ((_Pat)_Maybe) * (_Pat)_List
  | PXETag of _SrcLoc * _XName * (_PXAttr)_List * ((_Pat)_Maybe)
  | PXPcdata of _String
  | PXPatTag of _Pat
  | PXRPats of (_RPat)_List
  | PExplTypeArg of _QName * _Type
  | PQuasiQuote of _String * _String
  | PBangPat of _Pat
and _PatField = 
  | PFieldPat of _QName * _Pat
  | PFieldPun of _Name
  | PFieldWildcard
and _PXAttr = 
  | PXAttr of _XName * _Pat
and _RPat = 
  | RPOp of _RPat * _RPatOp
  | RPEither of _RPat * _RPat
  | RPSeq of (_RPat)_List
  | RPGuard of _Pat * (_Stmt)_List
  | RPCAs of _Name * _RPat
  | RPAs of _Name * _RPat
  | RPParen of _RPat
  | RPPat of _Pat
and _RPatOp = 
  | RPStar
  | RPStarG
  | RPPlus
  | RPPlusG
  | RPOpt
  | RPOptG
and _Literal = 
  | Char of _Char
  | String of _String
  | Int of _Integer
  | Frac of _Rational
  | PrimInt of _Integer
  | PrimWord of _Integer
  | PrimFloat of _Rational
  | PrimDouble of _Rational
  | PrimChar of _Char
  | PrimString of _String
and _ModuleName = ModuleName of _String
and _QName = 
  | Qual of _ModuleName * _Name
  | UnQual of _Name
  | Special of _SpecialCon
and _Name = 
  | Ident of _String
  | Symbol of _String
and _QOp = 
  | QVarOp of _QName
  | QConOp of _QName
and _Op = 
  | VarOp of _Name
  | ConOp of _Name
and _SpecialCon = 
  | UnitCon
  | ListCon
  | FunCon
  | TupleCon of _Boxed * _Int
  | Cons
  | UnboxedSingleCon
and _CName = 
  | VarName of _Name
  | ConName of _Name
and _IPName = 
  | IPDup of _String
  | IPLin of _String
and _XName = 
  | XName of _String
  | XDomName of _String * _String
and _Bracket = 
  | ExpBracket of _Exp
  | PatBracket of _Pat
  | TypeBracket of _Type
  | DeclBracket of (_Decl)_List
and _Splice = 
  | IdSplice of _String
  | ParenSplice of _Exp
and _Safety = 
  | PlayRisky
  | PlaySafe of _Bool
and _CallConv = 
  | StdCall
  | CCall
and _ModulePragma = 
  | LanguagePragma of _SrcLoc * (_Name)_List
  | OptionsPragma of _SrcLoc * ((_Tool)_Maybe) * _String
  | AnnModulePragma of _SrcLoc * _Annotation
and _Tool = 
  | GHC
  | HUGS
  | NHC98
  | YHC
  | HADDOCK
  | UnknownTool of _String
and _Rule = 
  | Rule of _String * _Activation * (((_RuleVar)_List)_Maybe) * _Exp * _Exp
and _RuleVar = 
  | RuleVar of _Name
  | TypedRuleVar of _Name * _Type
and _Activation = 
  | AlwaysActive
  | ActiveFrom of _Int
  | ActiveUntil of _Int
and _Annotation = 
  | Ann of _Name * _Exp
  | TypeAnn of _Name * _Exp
  | ModuleAnn of _Exp
and _SrcLoc = {srcFilename : _String ; srcLine : _Int ; srcColumn : _Int}
