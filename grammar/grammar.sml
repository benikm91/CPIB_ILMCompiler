datatype term
  = PROGRAM
  | IDENT
  | LBRACE
  | RBRACE
  | GLOBAL
  | LOCAL
  | SKIP
  | BECOMES
  | IF
  | ELSE
  | WHILE
  | LPAREN
  | RPAREN
  | CALL
  | DEBUGIN
  | DEBUGOUT
  | FOR
  | INIT
  | COMMA
  | BOOLOPR
  | RELOPR
  | ADDOPR
  | MULTOPR
  | FUN
  | SEMICOLON
  | FLOWMODE
  | CHANGEMODE
  | RETURNS
  | LITERAL
  | NOT
  | COLON
  | ATOMTYPE
  | LBRACKET
  | RBRACKET
  | PNTPNT
  | MECHMODE

val stringunderlineofunderlineterm = 
  fn PROGRAM => "PROGRAM"
  | IDENT => "IDENT"
  | LBRACE => "LBRACE"
  | RBRACE => "RBRACE"
  | GLOBAL => "GLOBAL"
  | LOCAL => "LOCAL"
  | SKIP => "SKIP"
  | BECOMES => "BECOMES"
  | IF => "IF"
  | ELSE => "ELSE"
  | WHILE => "WHILE"
  | LPAREN => "LPAREN"
  | RPAREN => "RPAREN"
  | CALL => "CALL"
  | DEBUGIN => "DEBUGIN"
  | DEBUGOUT => "DEBUGOUT"
  | FOR => "FOR"
  | SEMICOLON => "SEMICOLON"
  | INIT => "INIT"
  | BOOLOPR => "BOOLOPR"
  | RELOPR => "RELOPR"
  | ADDOPR => "ADDOPR"
  | MULTOPR => "MULTOPR"
  | FUN => "FUN"
  | FLOWMODE => "FLOWMODE"
  | COMMA => "COMMA"
  | CHANGEMODE => "CHANGEMODE"
  | RETURNS => "RETURNS"
  | LITERAL => "LITERAL"
  | NOT => "NOT"
  | COLON => "COLON"
  | ATOMTYPE => "ATOMTYPE"
  | LBRACKET => "LBRACKET"
  | RBRACKET => "RBRACKET"
  | PNTPNT => "PNTPNT"
  | MECHMODE => "MECHMODE"

datatype nonterm
  = program
    | cmd
    | cpsCmd
    | globInits
    | idents
    | expr
    | term1
    | term2
    | term3
    | decl
    | stoDecl
    | funDecl
    | globImps
    | globImp
    | cpsDecl
    | cpsStoDecl
    | underlinestoDeclMulti
    | underlinedeclMulti
    | underlineflowModeOptional
    | underlinecommaMulti
    | underlinechangeOptional
    | underlinereturnOptional
    | underlineglobalOptional
    | underlinelocalOptional
    | factor
    | exprList
    | monadicOpr
    | underlineexprOptional
    | underlineexprMulti
    | underlineinitOrExprListOptional
    | progParamList
    | progParam
    | paramList
    | param
    | typedIdent
    | underlineprogParamOptional
    | underlinechangeModeOptional
    | underlineparamOptional
    | underlinemechmodeOptional
    | underlinecommaMany
    | underlineprogParamMany

val stringunderlineofunderlinenonterm =
  fn program => "program"
    | cmd => "cmd"
    | cpsCmd => "cpsCmd"
    | globInits => "globInits"
    | idents => "idents"
    | expr => "expr"
    | term1 => "term1"
    | term2 => "term2"
    | term3 => "term3"
    | decl => "decl"
    | stoDecl => "stoDecl"
    | funDecl => "funDecl"
    | globImps => "globImps"
    | globImp => "globImp"
    | cpsDecl => "cpsDecl"
    | cpsStoDecl => "cpsStoDecl"
    | underlinestoDeclMulti => "underlinestoDeclMulti"
    | underlinedeclMulti => "underlinedeclMulti"
    | underlineflowModeOptional => "underlineflowModeOptional"
    | underlinecommaMulti => "underlinecommaMulti"
    | underlinechangeOptional => "underlinechangeOptional"
    | underlinereturnOptional => "underlinereturnOptional"
    | underlineglobalOptional => "underlineglobalOptional"
    | underlinelocalOptional => "underlinelocalOptional"
    | factor => "factor"
    | exprList => "exprList"
    | monadicOpr => "monadicOpr"
    | underlineexprOptional => "underlineexprOptional"
    | underlineexprMulti => "underlineexprMulti"
    | underlineinitOrExprListOptional => "underlineinitOrExprListOptional"
    | progParamList => "progParamList"
    | progParam => "progParam"
    | paramList => "paramList"
    | param => "param"
    | typedIdent => "typedIdent"
    | underlineprogParamOptional => "underlineprogParamOptional"
    | underlinechangeModeOptional => "underlinechangeModeOptional"
    | underlineparamOptional => "underlineparamOptional"
    | underlinemechmodeOptional => "underlinemechmodeOptional"
    | underlinecommaMany => "underlinecommaMany"
    | underlineprogParamMany => "underlineprogParamMany"

val stringunderlineofunderlinegramsym = (stringunderlineofunderlineterm, stringunderlineofunderlinenonterm)

local
  open FixFoxi.FixFoxiCore
in


val productions =
[
	(program,
		[[T PROGRAM, T IDENT, N progParamList, T LBRACE, N cpsCmd, T RBRACE],
		 [T PROGRAM, T IDENT, N progParamList, T GLOBAL, N cpsDecl, T LBRACE, N cpsCmd, T RBRACE]]),	 
	(cmd,
		[[T SKIP],
		 [N expr, T BECOMES, N expr],
		 [T IF, N expr, T LBRACE, N cpsCmd, T RBRACE, T ELSE, T LBRACE, N cpsCmd, T RBRACE],
		 [T WHILE, T LPAREN, N expr, T RPAREN, T LBRACE, N cpsCmd, T RBRACE],
		 [T CALL, T IDENT, N exprList],
		 [T CALL, T IDENT, N exprList, N globInits],
		 [T DEBUGIN, N expr],
		 [T DEBUGOUT, N expr],
		 [T FOR, T LPAREN, T IDENT, T RPAREN, T LBRACE, N cpsCmd, T RBRACE]]),
	(cpsCmd,
		[[N cmd],
		 [N cmd, T SEMICOLON, N cpsCmd]]),
	(globInits,
		[[T INIT, N idents]]),
	(idents,
		[[T IDENT],
		 [T IDENT, T COMMA, N idents]]),
	(expr,
		[[N term1],
		 [N term1, T BOOLOPR, N expr]]),
	(term1,
		[[N term2],
		 [N term2, T RELOPR, N term2]]),
	(term2,
		[[N term3],
		 [N term3, T ADDOPR, N term2]]),
	(term3,
		[[N factor],
		 [N factor, T MULTOPR, N term3]]),
	(decl, 
		[[N stoDecl], [N funDecl]]),
	(stoDecl, 
		[[N underlinechangeOptional, N typedIdent ], [N typedIdent]]),
	(funDecl, 
		[[T FUN, T IDENT, N paramList, N underlinereturnOptional, N underlineglobalOptional, N underlinelocalOptional, T LBRACE, N cpsCmd, T RBRACE]]),
	(globImps, 
		[[N globImp, N underlinecommaMulti]]),
	(globImp, 
		[[N underlineflowModeOptional, N underlinechangeOptional, T IDENT]]),
	(cpsDecl, 
		[[N decl, N underlinedeclMulti]]),
	(cpsStoDecl, 
		[[N stoDecl, N underlinestoDeclMulti]]),
	(underlinestoDeclMulti, 
		[[], [T SEMICOLON, N stoDecl, N underlinestoDeclMulti]]),
	(underlinedeclMulti, 
		[[], [T SEMICOLON, N decl, N underlinedeclMulti]]),
	(underlineflowModeOptional, 
		[[], [T FLOWMODE]]),
	(underlinecommaMulti, 
		[[], [T COMMA, N globImp, N underlinecommaMulti]]),
	(underlinechangeOptional, 
		[[], [T CHANGEMODE]]),
	(underlinereturnOptional, 
		[[], [T RETURNS, N stoDecl]]),
	(underlineglobalOptional, 
		[[], [T GLOBAL, N globImps]]),
	(underlinelocalOptional, 
		[[], [T LOCAL, N cpsStoDecl]]),
	(factor, 
		[[T LITERAL], 
		 [T IDENT, N underlineinitOrExprListOptional], 
		 [N monadicOpr, N factor], 
		 [T LPAREN, N expr, T RPAREN]]),
	(exprList, 
		[[T LPAREN, N underlineexprOptional, T RPAREN]]),
	(monadicOpr, 
		[[T NOT, T ADDOPR]]),
	(underlineexprOptional, 
		[[], [N expr, N underlineexprMulti]]),
	(underlineexprMulti, 
		[[], [T COMMA, N expr, N underlineexprMulti]]),
	(underlineinitOrExprListOptional, 
		[[], [T INIT], [N exprList]]),
	(progParamList, 
		[[T LPAREN, N underlineprogParamOptional, T RPAREN]]),
	(progParam, 
		[[N underlineflowModeOptional, N underlinechangeModeOptional, T typedIdent]]),
	(paramList, 
		[[T LPAREN, N underlineparamOptional, T RPAREN]]),
	(param, 
		[[N underlineflowModeOptional, N underlinemechmodeOptional, N underlinechangeModeOptional, N typedIdent]]),
	(typedIdent, 
		[[T IDENT, T COLON, T ATOMTYPE],
		[T IDENT, T COLON, T ATOMTYPE, T LBRACKET, N expr, T PNTPNT, N expr, T RBRACKET],
		[T IDENT, T COLON, T ATOMTYPE, T LPAREN, N expr, T PNTPNT, N expr, T RPAREN]]),
	(underlineprogParamOptional, 
		[[N progParam, N underlineprogParamMany], 
		 []]),
	(underlinechangeModeOptional, 
		[[T CHANGEMODE],
		 []]),
	(underlineparamOptional, 
		[[N param, N underlinecommaMany],
		 []]),
	(underlinemechmodeOptional, 
		[[N MECHMODE],
		 []]),
	(underlinecommaMany, 
		[[T COMMA, N param, N underlinecommaMany],
		 []]),
	(underlineprogParamMany, 
		[[T COMMA, N progParam, N underlineprogParamMany], 
		 []])
]

val S = program

val result = fix_foxi productions S string_of_gramsym

end (* local *)
