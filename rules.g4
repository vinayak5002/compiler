grammar rules;

module					:	module_declarations body '$'
						|	body '$';

body					:	import_statements top_declarations
						| 	top_declarations;

module_declarations		:	MODULE qvar export_list WHERE;

export_list				:	LPAREN export_items RPAREN
						|	;

export_items			:	export_item COMMA export_items
						|	export_item;
export_item				:	qvar | qcon LPAREN mon_con_list RPAREN;

mon_con_list			:	qvcon COMMA
						|	qvcon;

//Import statements
import_statements		:	module_import
						|	module_import_list;

module_import_list		:	module_import
						|	module_import COMMA module_import_list;

module_import			:	QUALIFIED UID
						|	UID
						|	QUALIFIED qcon AS UID
						|	import_spec;

import_spec				:	LPAREN import_list RPAREN
						|	LPAREN QUALIFIED import_list RPAREN
           				| 	LPAREN HIDING import_list RPAREN
       					| 	LPAREN QUALIFIED AS ID import_list RPAREN
   						| 	LPAREN HIDING QUALIFIED import_list RPAREN
   						|	;

import_list				:	import_item
						|	import_item COMMA import_list;

import_item				:	qcon
           				| 	qcon LPAREN constructor_list RPAREN
           				| 	HIDING LPAREN import_list RPAREN
           				| 	HIDING QUALIFIED LPAREN import_list RPAREN
       					| 	HIDING QUALIFIED AS qcon LPAREN import_list RPAREN
           				| 	QUALIFIED LPAREN import_list RPAREN
           				| 	QUALIFIED AS ID LPAREN import_list RPAREN;

constructor_list		:	ID COMMA constructor_list
						|	ID;

qvar 					:	UID DOT ID | ID;
qcon 					:	UID DOT UID | UID;
qvcon 					:	UID | ID;
// qcon					:	UID DOT ID | ID;

//Declarations

top_declarations		: top_declaration top_declarations
						| ;

top_declaration			:	data_declarations
						|	newtype_declarations deriving
		    			| 	data_declarations deriving
		    			| 	class_declarations
		    			| 	instance_declarations
		    			| 	default_declarations
		    			| 	foreign_import
		    			| 	declarations;

deriving				:	DERIVING LPAREN dclasses RPAREN;
dclasses				:	qcon COMMA dclasses | qcon;

// Data declarations
data_declarations		:	DATA qcon ASSIGN constrs;

constrs					:	UID
						|	UID contypes
						|	UID	LFLOWER fields RFLOWER;

fields 					:	field COMMA fields | field;
field					:	ID DOUBLE_COLON type;


contypes				:	type VBAR contypes | type;

// New type declarations

newtype_declarations	:	NEWTYPE qcon ASSIGN new_constr;
new_constr				:	UID type
						|	UID LFLOWER ID DOUBLE_COLON type RFLOWER;

// Class Declarations
class_declarations		:	CLASS qcon ID WHERE cdecls;

cdecls					:	cdecl cdecls | cdecl;

cdecl					:	vars DOUBLE_COLON type
						|	vars DOUBLE_COLON context DOUBLEARROW type;

context					:	classes
						|	LPAREN classes RPAREN;

classes					:	cls COMMA classes | cls;

cls						: 	qcon ID
						|	qcon ID type;

// Instance declaration
instance_declarations	:	INSTANCE qcon inst WHERE idecls
						|	INSTANCE scontext DOUBLEARROW inst WHERE idecls;

inst					:	qcon
						|	LPAREN qcon vars RPAREN
						| 	LPAREN vars RPAREN
						|	LBRACKET ID RBRACKET;

scontext				:	qcon
						|	LPAREN simple_classes RPAREN;

simple_classes			:	qcon ID COMMA simple_classes
						|	qcon ID;

idecls					:	idecl idecls | idecl;
idecl					:	funlhs rhs
						|	ID rhs;

vars					:	ID vars | ID;

// Default declarations
default_declarations	:	DEFAULT LPAREN types RPAREN;

// Declarations
declarations			: 	decl declarations | decl;

decl					:	gendecl
						|	funlhs rhs
						|	pat rhs;

funlhs					:	ID pats
						|	ID LPAREN pats RPAREN
						|	ID LPAREN funlhs RPAREN pats;

rhs						:	ASSIGN expression
						|	ASSIGN expression WHERE declarations
						|	gdrhs
						| 	gdrhs WHERE declarations;

gdrhs					:	gd ASSIGN expression gdrhs
                        |   ;

gd						:	VBAR exp1;

exp1					:	expression DOUBLE_COLON DOUBLEARROW type
						|	expression;

pats					:	pat pats | pat;

pat						:	ID
						|	qcon
						|	literal
						|	WILDCARD
						| 	LPAREN pats RPAREN
						|	LBRACKET pats RBRACKET
						|	pat COLON;

gendecl					:	ID DOUBLE_COLON context DOUBLEARROW
						|	ID DOUBLE_COLON types
						|	fixity INTEGER ops;

ops						:	op COMMA ops | op;

fixity					:	INFIXL | INFIXR | INFIX;

// Foreign imports
foreign_import			:	FOREIGN IMPORT calling_convention STRING AS qcon LPAREN types RPAREN;

calling_convention		:	STDCALL
                  		| 	CCALL
                  		| 	CAPI
	      				| 	CPPCALL
      					| 	JSCALL
	      				|	REC;

types 					:	type types
                        |   type;

type					:	atype
						|	ARROW type;

atype					:	qcon
						|	qcon LPAREN RPAREN
                        |   LPAREN types RPAREN
						|	LPAREN type_list RPAREN
						|	LBRACKET atype RBRACKET;

type_list				:	type COMMA type_list | type;

// Lambda
lambda					:	BACKSLASH pats ARROW expression;

if_statement			:	IF expression THEN expression ELSE expression;

case_statement			:	CASE expression OF case_alternatives;
case_alternatives		:	case_alternative case_alternatives
						|	case_alternative;
case_alternative		:	pat ARROW expression;

let_statement			:	LET declarations IN expression;

do_block				:	DO stmts;

stmts					:	stmt stmts | stmt;

stmt					:	expression
						|	pat ARROW expression
						|	LET declarations;

expression				:	lambda
       					| 	if_statement
				       	| 	case_statement
				       	| 	let_statement
				       	| 	do_block
						| 	ID
			       		| 	literal
						| 	function_application
						| 	list_comprehensions
						| 	expression op rexp
						| 	MINUS expression;

function_application	: 	ID args
						|	ID LPAREN args RPAREN;

args					:	expression COMMA args
						|	expression
						| 	function_application;

list_comprehensions		:	LPAREN expression RPAREN
						| 	LPAREN exps RPAREN
						| 	LBRACKET expression RANGE RBRACKET
						| 	LBRACKET expression RANGE expression RBRACKET
						| 	LBRACKET expression COMMA expression DOUBLE_DOT expression RBRACKET
						| 	LBRACKET expression VBAR gens RBRACKET;

gens					:	gen COMMA gens | gen;
gen						:	pat ARROW expression
						|	LET declarations
						|	expression;

exps 					:	expression COMMA exps | expression;

rexp					:	expression;

//lexp 					:	lexp op expression
//          				|	expression;
//rexp					:	expression op rexp
//          				|	expression;

op						: 	PLUS
						| 	MINUS
						| 	MULT
						| 	DIV
						| 	EQUALS
						| 	NOT_EQUALS
						| 	LESS_THAN
						| 	GREATER_THAN
						| 	LESS_THAN_EQUAL
						| 	GREATER_THAN_EQUAL
						| 	AND
						| 	OR
						| 	COLON
						| 	CONCAT;

literal					: 	INTEGER
						| 	FLOAT
						| 	CHAR
						| 	STRING;

//KEYWORDS:
AS   		 	: 'as'      		 	;
CASE 		 	: 'case'    		 	;
DEFAULT  	 	: 'default' 		 	;
DO   		 	: 'do'      		 	;
ELSE 		 	: 'else'    		 	;
HIDING   	 	: 'hiding'  		 	;
IF   		 	: 'if'      		 	;
IMPORT   	 	: 'import'  		 	;
IN   		 	: 'in'      		 	;
INFIX		 	: 'infix'   		 	;
INFIXL   	 	: 'infixl'  		 	;
INFIXR   	 	: 'infixr'  		 	;
LET  		 	: 'let'     		 	;
MODULE   	 	: 'module'  		 	;
OF   		 	: 'of'      		 	;
QUALIFIED    	: 'qualified'   	 	;
THEN 		 	: 'then'    		 	;
WHERE		 	: 'where'   		 	;
WILDCARD 	 	: '_'       		 	;
FORALL   	 	: 'forall'  		 	;
FOREIGN  	 	: 'foreign' 		 	;
EXPORT   	 	: 'export'  		 	;
SAFE 		 	: 'safe'    		 	;
INTERRUPTIBLE 	: 'interruptible'       ;
UNSAFE   	 	: 'unsafe'  		 	;
MDO  		 	: 'mdo'     		 	;
FAMILY   	 	: 'family'  		 	;
ROLE 		 	: 'role'    		 	;
STDCALL  	 	: 'stdcall' 		 	;
CCALL		 	: 'ccall'   		 	;
CAPI 		 	: 'capi'    		 	;
CPPCALL  	 	: 'cplusplus'   	 	;
JSCALL   	 	: 'javascript'  	 	;
REC  		 	: 'rec'     		 	;
GROUP	  	 	: 'group'			 	;
BY   		 	: 'by'      		 	;
USING		 	: 'using'   		 	;
PATTERN  	 	: 'pattern' 		 	;
STOCK		 	: 'stock'   		 	;
ANYCLASS 	 	: 'anyclass'		 	;
VIA  		 	: 'via'     		 	;


//OBJECT RELATED KEYWORDS: (justification provided in text)
DATA			: 'data'				;
TYPE			: 'type'				;
CLASS			: 'class'				;
INSTANCE		: 'instance'			;
NEWTYPE			: 'newtype'				;
DERIVING		: 'deriving'			;

//OPERATORS:
PLUS			: '+'					;
MINUS			: '-'					;
MULT			: '*'					;
DIV				: '/'					;
MOD				: '%'					;
EQUALS 			: '=='					;
NOT_EQUALS		: '/='					;
LESS_THAN		: '<'					;
GREATER_THAN	: '>'					;
LESS_THAN_EQUAL : '<='					;
GREATER_THAN_EQUAL : '>='				;
AND				: '&&'					;
OR				: '||'					;
NOT				: 'not'					;
BITWISE_AND		: '&'					;
VBAR	 		: '|'					;
XOR 			: 'xor'					;
ASSIGN 			: '='					;
DOT 			: '.'					;
COLON 			: ':'					;
CONCAT 			: '++'					;

//SYMBOLS:
DOLLAR 			: '$'					;
LBRACKET 		: '['					;
RBRACKET 		: ']'					;
RANGE 			: '..'					;
BACKSLASH 		: '\\'					;
LPAREN 			: '('					;
RPAREN 			: ')'					;
COMMA			: ','					;
BACKTICK 		: '`'					;
SEMICOLON		: ';'					;
LFLOWER			: '{'					;
RFLOWER			: '}'					;
DOUBLEARROW		: '=>'					;
RARROW			: '<-'					;

// Type Annotations and Bindings
DOUBLE_COLON 	: '::'					;
BIND_RIGHT 		: '>>='					;
BANG 			: '!'					;

// Other Operators
HASH 			: '#'					;
QUESTION 		: '?'					;
AT 				: '@'					;
CARET 			: '^'					;
TILDE 			: '~'					;
DEFINE 			: ':='					;

// White Characters:
WS 				: [ \t]+ -> skip		;
NEWLINE 		: '\r'? '\n' -> skip	;
TAB 			: [\t]+					;

//COMMENTS:
COMMENT 		: '--' ~[\r\n]* -> skip	;
MULTI_LINE_COMMENT: '{-' .*? '-}'->skip	;

//IDENTIFIERS:
UID				: [A-Z][a-zA-Z0-9]*  ;
ID 				: [a-zA-Z][a-zA-Z0-9]*	;

//CONSTANTS
INTEGER 		: [0-9]+															;
FLOAT 			: ([0-9]+ '.' [0-9]* | '.' [0-9]+ | [0-9]+) ([eE] [+\-]? [0-9]+)?	;
CHAR 			: '\'' ~'\'' '\''													;
STRING 			: '"' ( '\\' . | ~('\n'|'\r'|'"') )* '"'							;

//OTHER TOKENS:
ARROW 			: '->'					;
