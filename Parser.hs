
module Parser where

import Text.ParserCombinators.Parsec

{-
  // There are some special hard-wired unary operators:
  // * (dereference ptr or const)
  // There are some special hard-wired binary operators:
  // && (short-circuit logical conjunction)
  // || (short-circuit logical disjunction)
  // == (structural equality over all types)
  // != (structural inequality over all types)
  // <, >, <=, >= (structural comparison over all types which don't contain pointers)
  // <=> (structural comparison, as above, but returning a 3-valued-enum result)
-}

simple_name 
	:	 ID^
	;

end!: ';'
	;

dot!:	'.'
	;

comma!
	:	','
	;

lparen!
	:	'('
	;

rparen!
	:	')'
	;

lbracket!
	:	'{'
	;

rbracket!
	:	'}'
	;

colon!
	:	':'
	;
	
equals!
	:	'='
	;
	
arrow!
	:	'=>'
	;

name : (simple_name dot)* simple_name
;

params : lparen (param (comma param)* comma?)? rparen
;

param : simple_name (colon type)?
;

type_def
{-
	// The arguments may be given for enum or mask to specify the backing type.
	// In a struct or union, all elements must have a type; in an enum or mask, none must.
	// In a struct, all elements must have an initial value; in a union, exactly one must.
	// In an enum or mask, the parameters must all be static.
	// In an enum or mask, any element may have an initial value, but the initial values must all be valid.
-}
	:	('struct' | 'union' | 'enum' | 'mask') arguments? name params? lbracket struct_elems rbracket end
	;

struct_elems
	:	(struct_elem (comma struct_elem)* comma?)?
	;

struct_elem : simple_name (colon type)? (equals expr)?
;

type :
{-
  // There are some special hard-wired names that can go here:
  // ptr (pointer, also used for arrays)
  // const (like ptr, but read-only)
  // tag (get tag type of union)
  // and all the primitive types
-}
    name arguments?
  | params arrow type
;

expr_head
	:	literal
{-
	// There are some special hard-wired names that can go here:
  // length (get length of array)
  // tag (get tag value of union)
  // ref (increment reference count)
  // unref (decrement reference count)
  // const (safe cast from ptr to const)
  // cast (unsafe cast to any specified type)
  // first, last, iteration (get information about loop iterations)
-}
	|	name
	|	OPERATOR expr_head
	|	lparen expr rparen
	;

expr_tail
	:	arguments
	|	OPERATOR expr
	;

expr:	expr_head expr_tail*
	;

arguments : lparen (expr (comma expr)* comma?)? rparen
;

literal
	:	INT
	|	FLOAT
	|	CHAR
	|	STRING
  	|	'new' name arguments
  	|	params colon type arrow fn_body -> ^(Fn params type fn_body)
	;

fn_body : expr^ | block^
;

block : lbracket (stmt*)^ rbracket
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
  | 'for' lparen loop_var 'in'! expr rparen block ('next' block)?
  | ('next' | 'done' | 'return') end
  | 'decide' arguments lbracket option* rbracket
  | 'do' block
;

option_expr
	:	expr
	|	'_'
	;

option : lparen (option_expr (comma option_expr)* comma?)? rparen arrow (expr end | block)
;

loop_var
	:	simple_name colon type equals expr
	;

def :	'def' simple_name def_tail -> ^(Def simple_name def_tail)
	;
	
expr_or_block
	:	expr^ end
	|	block^
	;

def_tail
	:	equals expr^
	|	params colon type equals expr_or_block -> ^(Fn params type expr_or_block)
	;

ns :
    file_ns
  | block_ns
;

file:	file_ns^
	|	ns_elem* -> ^(Namespace Main ns_elem*)
	;

file_ns : 'ns' simple_name end ns_elem* -> ^(Namespace simple_name ns_elem*)
;

block_ns : 'ns' simple_name lbracket ns_elem* rbracket -> ^(Namespace simple_name ns_elem*)
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

imp : 'import' ((simple_name comma)* simple_name comma?)? 'from'! name end
;

