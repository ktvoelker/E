grammar E;

ID  :	('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*
    ;

INT :	'0'..'9'+
    ;

FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;

COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {$channel=HIDDEN;}
    ;

STRING
    :  '"' ( ESC_SEQ | ~('\\'|'"') )* '"'
    ;

CHAR:  '\'' ( ESC_SEQ | ~('\''|'\\') ) '\''
    ;

fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F') ;

fragment
ESC_SEQ
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UNICODE_ESC
    |   OCTAL_ESC
    ;

fragment
OCTAL_ESC
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UNICODE_ESC
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

OPERATOR
	:	('~'|'!'|'@'|'$'|'%'|'^'|'&'|'*'|'-'|'+'|'='|'|'|'/'|'<'|'>'|'?')+
	;

  // There are some special hard-wired unary operators:
  // * (dereference ptr or const)
  // There are some special hard-wired binary operators:
  // && (short-circuit logical conjunction)
  // || (short-circuit logical disjunction)
  // == (structural equality over all types)
  // != (structural inequality over all types)
  // <, >, <=, >= (structural comparison over all types which don't contain pointers)
  // <=> (structural comparison, as above, but returning a 3-valued-enum result)
  
file:	file_ns
	|	ns_elem*
	;

simple_name 
	:	 ID
	;

end : ';'
;

name : (simple_name '.')* simple_name
;

params : '(' (param (',' param)* ','?)? ')'
;

param : simple_name (':' type)?
;

type_def :
    'struct' name params? '{' struct_elems '}' end
  // Only one of the union_elems can have an initial value.
  | 'union' name params? '{' union_elems '}' end
  // The arguments are just for specifying the backing type of the enum or mask,
  // but we can give better error messages by accepting any valid argument list at
  // the syntactic level.
  // Also, note that all the params must be static.
  | ('enum' | 'mask') arguments? name params? '{' enum_elems '}' end
;

struct_elems
	:	(struct_elem (',' struct_elem)* ','?)?
	;

struct_elem : simple_name ':' type '=' expr
;

union_elems
	:	(union_elem (',' union_elem)* ','?)?
	;

union_elem : simple_name ':' type ('=' expr)?
;

enum_elem : simple_name ('=' expr)?
;

enum_elems 
	:	(enum_elem (',' enum_elem)* ','?)?
	;

type :
  // There are some special hard-wired names that can go here:
  // ptr (pointer, also used for arrays)
  // const (like ptr, but read-only)
  // tag (get tag type of union)
  // and all the primitive types
    name arguments?
  | params '=>' type
;

expr_head
	:	literal
	// There are some special hard-wired names that can go here:
  	// length (get length of array)
  	// tag (get tag value of union)
  	// ref (increment reference count)
  	// unref (decrement reference count)
  	// const (safe cast from ptr to const)
  	// cast (unsafe cast to any specified type)
  	// first, last, iteration (get information about loop iterations)
	|	name
	|	OPERATOR expr_head
	|	'(' expr ')'
	;

expr_tail
	:	arguments
	|	OPERATOR expr
	;

expr:	expr_head expr_tail*
	;

arguments : '(' (expr (',' expr)* ','?)? ')'
;

literal
	:	INT
	|	FLOAT
	|	CHAR
	|	STRING
  	|	'new' name arguments
  	|	params ':' type '=>' fn_body
	;

fn_body : expr | block
;

block : '{' stmt* '}'
;

stmt :
    expr end
  | def
  // The initial "with" expression is a predicate function applied to each test
  // expression. By default, the identity function over Booleans is used.
  | ('with' expr)? 'if' expr block ('elif' expr block)* ('else' block)?
  | 'while' expr block ('next' block)?
  // The for-each loop expression must be of a type for which all the necessary
  // functions are defined to allow it to be used as an iterable thing.
  | 'for' '(' loop_var 'in' expr ')' block ('next' block)?
  | ('next' | 'done' | 'return') end
  | 'decide' arguments '{' option* '}'
  | 'do' block
;

option_expr
	:	expr
	|	'_'
	;

option : '(' (option_expr (',' option_expr)* ','?)? ')' '=>' (expr end | block)
;

loop_var
	:	simple_name ':' type '=' expr
	;

def : 'def' simple_name '=' (expr end | block)
;

ns :
    file_ns
  | block_ns
;

file_ns : 'ns' simple_name end ns_elem*
;

block_ns : 'ns' simple_name '{' ns_elem* '}'
;

ns_elem :
    block_ns
  | def_mod* def
  | imp
;

def_mod :
    'export'
  | 'global'
;

imp : 'import' ((simple_name ',')* simple_name ','?)? 'from' name end
;
