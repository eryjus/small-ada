   /******* A YACC grammar for Ada 9X *********************************/
/* Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        */
/* Copying permitted if accompanied by this statement.             */
/* Derivative works are permitted if accompanied by this statement.*/
/* This grammar is thought to be correct as of May 1, 1994         */
/* but as usual there is *no warranty* to that effect.             */
/*******************************************************************/

%{
    #include "small-ada-tree.h"

	#include <cstdio>
	#include <cstring>

	extern int parse_error;
	extern int yylex(void);
	extern int openBuffer(const char *name);

	#define yyerror(x) do { fprintf(stderr, "%s[%d@%d]: %s\n", __FILE__, __LINE__, curr_lineno, (x), "\n"); parse_error ++; } while (0)

	extern AdaPgm *ast;

    const char *standard = "/home/adam/workspace/small-ada/std-packages/standard.ads";
    string pkgLib = "/home/adam/workspace/small-ada/std-packages/";

    inline const char *MakePkgFile(string pkg) { return (pkgLib + pkg + ".ads").c_str(); }
%}

// %define api.pure

%token TIC
%token DOT_DOT
%token LT_LT
%token BOX
%token LT_EQ
%token EXPON
%token NE
%token GT_GT
%token GE
%token IS_ASSIGNED
%token RIGHT_SHAFT
%token ABORT
%token ABS
%token ABSTRACT
%token ACCEPT
%token ACCESS
%token ALIASED
%token ALL
%token AND
%token ARRAY
%token AT
%token BEGiN
%token BODY
%token CASE
%token CONSTANT
%token DECLARE
%token DELAY
%token DELTA
%token DIGITS
%token DO
%token ELSE
%token ELSIF
%token END
%token ENTRY
%token EXCEPTION
%token EXIT
%token FOR
%token FUNCTION
%token GENERIC
%token GOTO
%token IF
%token IN
%token IS
%token LIMITED
%token LOOP
%token MOD
%token NEW
%token NOT
%token NuLL
%token OF
%token OR
%token OTHERS
%token OUT
%token PACKAGE
%token PRAGMA
%token PRIVATE
%token PROCEDURE
%token PROTECTED
%token RAISE
%token RANGE
%token RECORD
%token REM
%token RENAMES
%token REQUEUE
%token RETURN
%token REVERSE
%token SELECT
%token SEPARATE
%token SUBTYPE
%token TAGGED
%token TASK
%token TERMINATE
%token THEN
%token TYPE
%token UNTIL
%token USE
%token WHEN
%token WHILE
%token WITH
%token XOR
%token char_lit
%token identifier
%token char_string
%token numeric_lit

%token ERROR

// -- the following are virtual tokens and are not used in the lexical analyzer
//    -------------------------------------------------------------------------

%token AND_THEN
%token OR_ELSE
%token NOT_IN
%token LT
%token GT
%token EQ
%token IDENTITY
%token NEGATE
%token PLUS
%token MINUS
%token CONCAT
%token MUL
%token DIV

%union {
	const char *error_msg;
	IdentEntry *id;
	NumberEntry *nbr;
	StringEntry *str;
	int character;

	int token;

	AdaPgm *adaPgm;
	BlockBody *body;
	QualNameList *qNameList;
	CSymbolList *cSymList;
	Declaration *decl;
	DeclarationList *declList;
	Expression *expr;
	ExpressionList *exprList;
	Identifier *ident;
	Statement *stmt;
	StatementList *stmtList;
	Label *lbl;


	FormalParms *parms;
	ExceptionList *except;
}

%type	<id>			identifier
%type	<nbr>			numeric_lit
%type	<str>			char_string
%type	<character>		char_lit used_char


%type	<token>			adding logical multiplying relational unary

%type	<adaPgm>		goal_symbol

%type	<body>			block_body handled_stmt_s

%type	<qNameList>		c_name_list

%type	<decl>			pkg_decl pkg_spec private_opt subprog_body
%type	<decl>			subprog_spec subprog_spec_is_push use_clause

%type	<declList>		body compilation comp_unit context_spec decl_item_s decl_item_s1
%type	<declList>		decl_part pragma_s private_part
%type	<declList>		unit use_clause_opt with_clause

%type	<expr>			expression factor indexed_comp literal name
%type	<expr>			operator_symbol parenthesized_primary
%type	<expr>			primary relation simple_expression term value

%type	<exprList>		name_s value_s

%type   <ident>         c_id_opt compound_name designator id_opt simple_name

%type	<lbl>			label

%type	<stmt>			assign_stmt compound_stmt null_stmt procedure_call
%type	<stmt>			simple_stmt statement unlabeled

%type	<stmtList>		statement_s



%type	<except>		except_handler_part_opt except_handler_part
%type	<parms>			formal_part_opt

%%

// ============================================================================================
// ============================================================================================

goal_symbol
	:   compilation {
			$$ = AdaPgm::factory($1);
			ast = $$;
		}
	;

// ============================================================================================
// ============================================================================================

compilation
	:	// empty!!
		{
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| compilation comp_unit
		{
			$$ = append($1, $2);
		}

// --------------------------------------------------------------------------------------------

	| pragma pragma_s
		{
			yyerror("Unimplemented feature: [compilation := pragma pragma_s]");
			$$ = DeclarationList::empty();
		}
	;

// ============================================================================================
// ============================================================================================

pragma_s
	:	// empty!!
		{
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| pragma_s pragma
		{
			yyerror("Unimplemented feature: [pragma_s := pragma_s pragma]");
			$$ = DeclarationList::empty();
		}
	;

// ============================================================================================
// ============================================================================================

pragma
	: PRAGMA identifier ';'
		{
			yyerror("Unimplemented feature: [pragma := PRAGMA identifier ';']");
		}

// --------------------------------------------------------------------------------------------

	| PRAGMA simple_name '(' pragma_arg_s ')' ';'
		{
			yyerror("Unimplemented feature: [pragma := PRAGMA simple_name '(' pragme_arg_s ')' ';'");
		}
	;

// ============================================================================================
// ============================================================================================

pragma_arg_s
	: pragma_arg
		{
			yyerror("Unimplemented feature: [pragma_arg_s := pragma_arg]");
		}

// --------------------------------------------------------------------------------------------

	| pragma_arg_s ',' pragma_arg
		{
			yyerror("Unimplemented feature: [pragma := pragma_arg_s ',' pragma_arg]");
		}
	;

// ============================================================================================
// ============================================================================================

pragma_arg
	: expression
		{
			yyerror("Unimplemented feature: [pragme_arg := expression]");
		}

// --------------------------------------------------------------------------------------------

	| simple_name RIGHT_SHAFT expression
		{
			yyerror("Unimplemented feature: [pragma_arg := simple_name RIGHT_SHAFT expression]");
		}
	;

// ============================================================================================
// ============================================================================================

comp_unit
	: context_spec private_opt unit pragma_s
		{
			if ($2) $$ = append(append(append($1, DeclarationList::factory($2)), $3), $4);
			else $$ = append(append($1, $3), $4);
		}

// --------------------------------------------------------------------------------------------

	| private_opt unit pragma_s
		{
			if ($1) $$ = append(append(DeclarationList::factory($1), $2), $3);
			else $$ = append($2, $3);
		}
	;

// ============================================================================================
// ============================================================================================

private_opt
	:	// empty!!
		{
			$$ = PrivateDeclaration::empty();
		}

// --------------------------------------------------------------------------------------------

	| PRIVATE
		{
			$$ = PrivateDeclaration::factory();
		}
	;

// ============================================================================================
// ============================================================================================

context_spec
	: with_clause use_clause_opt
		{
			$$ = append($1, $2);
		}

// --------------------------------------------------------------------------------------------

	| context_spec with_clause use_clause_opt
		{
			$$ = append(append($1, $2), $3);
		}

// --------------------------------------------------------------------------------------------

	| context_spec pragma
		{
            $$ = $1;
//			$$ = append($1, $2);
		}
	;

// ============================================================================================
// ============================================================================================

with_clause
	: WITH c_name_list ';'
		{
            // The following have to be done at this point (need to check documentation):
            // 1. Prepend the standard pacakge on the list, if it is not already there
            // 2. For each item in c_name_list, open the file and parse it
            //     !!! This means yyparse recursion
            // 3. yyparse() will return a ast pointer, which is of type AdaPgm.  We only need
            //    the DeclarationList part of this pointer -- so we will need to free the
            //    AdaPgm object.
            // 4. Of course, add the context to the DeclarationList
            // ------------------------------------------------------------------------------

            DeclarationList *wrk = DeclarationList::empty();

            for (QualNameList *l = first($2); more(l); l = next(l)) {
                ast = AdaPgm::empty();
                Identifier *d = l->Get_names();
                string fn = d->Get_id()->GetKeyValue();

                if (strncmp(fn.c_str(), "ada.", 4) != 0) {
                    fn = string("ada.") + fn;
                }

cout << "Opening... " << MakePkgFile(fn) << endl;

                if (openBuffer(MakePkgFile(fn))) {
                    cout << "File open!\n";
//                    yyparse();
                    wrk = append(wrk, ast->GetDeclsAndDestroy());
                    ast = AdaPgm::empty();
                } else yyerror ("Unable to open file");
            }

			$$ = DeclarationList::factory(WithDeclaration::factory($2));
			$$ = append($$, wrk);
		}
	;

// ============================================================================================
// ============================================================================================

c_name_list
	: compound_name
		{
			$$ = QualNameList::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| c_name_list ',' compound_name
		{
			$$ = append($1, QualNameList::factory($3));
		}
	;

// ============================================================================================
// ============================================================================================

compound_name
	: simple_name
		{
            $$ = $1;
        }

// --------------------------------------------------------------------------------------------

	| compound_name '.' simple_name
		{
            Symbol sym = idTable.AddString($1->GetIDString() + "." + $3->GetIDString());
            $1->Set_id(sym);
		}
	;

// ============================================================================================
// ============================================================================================

simple_name
	: identifier
		{
			$$ = Identifier::factory($1);
		}
	;

// ============================================================================================
// ============================================================================================

use_clause_opt
	:	// empty!!
		{
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| use_clause_opt use_clause
		{
			$$ = append($1, DeclarationList::factory($2));
		}
	;

// ============================================================================================
// ============================================================================================

use_clause
	: USE name_s ';'
		{
			$$ = UseDeclaration::factory($2);
		}

// --------------------------------------------------------------------------------------------

	| USE TYPE name_s ';'
		{
			$$ = UseTypeDeclaration::factory($3);
		}
	;

// ============================================================================================
// ============================================================================================

name_s
	: name
		{
			$$ = ExpressionList::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| name_s ',' name
		{
			$$ = append($1, ExpressionList::factory($3));
		}
	;

// ============================================================================================
// ============================================================================================

unit
	: pkg_decl
		{
			$$ = DeclarationList::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| pkg_body
		{
			yyerror("Unimplemented feature: [unit : pkg_body]");
            $$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| subprog_decl
		{
			yyerror("Unimplemented feature: [unit : subprog_decl]");
            $$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| subprog_body
		{
			$$ = DeclarationList::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| subunit
		{
			yyerror("Unimplemented feature: [unit : subunit]");
            $$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| generic_decl
		{
			yyerror("Unimplemented feature: [unit : generic_decl]");
            $$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| rename_unit
		{
			yyerror("Unimplemented feature: [unit : rename_unit]");
            $$ = DeclarationList::empty();
		}
	;

// ============================================================================================
// ============================================================================================

decl_item_s
	:	// empty!!
		{
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| decl_item_s1
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

decl_item_s1
	: decl_item
		{
			yyerror("Unimplemented feature: [decl_item_s1 := decl_item]");
		}

// --------------------------------------------------------------------------------------------

	| decl_item_s1 decl_item
		{
			yyerror("Unimplemented feature: [decl_item_s1 := decl_item_s1 decl_item]");
		}
	;

// ============================================================================================
// ============================================================================================

decl_item
	: decl
		{
			yyerror("Unimplemented feature: [decl_item := decl]");
		}

// --------------------------------------------------------------------------------------------

	| use_clause
		{
			yyerror("Unimplemented feature: [decl_item := use_clause]");
		}

// --------------------------------------------------------------------------------------------

	| rep_spec
		{
			yyerror("Unimplemented feature: [decl_item := rep_spec]");
		}

// --------------------------------------------------------------------------------------------

	| pragma
		{
			yyerror("Unimplemented feature: [decl_item := pragma]");
		}
	;

// ============================================================================================
// ============================================================================================

pkg_decl
	: pkg_spec ';'
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| generic_pkg_inst ';'
		{
			yyerror("Unimplemented feature: [pkg_decl : generic_pkg_inst ';']");
		}
	;

// ============================================================================================
// ============================================================================================

pkg_spec
	: PACKAGE compound_name IS decl_item_s private_part END c_id_opt
		{
			$$ = Package::factory($2, $4, $5, $7);
		}
	;

// ============================================================================================
// ============================================================================================

private_part
	:	// empty!!
		{
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| PRIVATE decl_item_s
		{
			$$ = $2;
		}
	;

// ============================================================================================
// ============================================================================================

c_id_opt
	:	// empty!!
		{
            $$ = Identifier::empty();
		}

// --------------------------------------------------------------------------------------------

	| compound_name
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

generic_pkg_inst
	: PACKAGE compound_name IS generic_inst
		{
			yyerror("Unimplemented feature: [generic_pkg_inst := PACKAGE compound_name IS generic_inst]");
		}
	;

// ============================================================================================
// ============================================================================================

generic_inst
	: NEW name
		{
			yyerror("Unimplemented feature: [generic_inst := NEW name]");
		}
	;

// ============================================================================================
// ============================================================================================

pkg_body
	: PACKAGE BODY compound_name IS decl_part body_opt END c_id_opt ';'
		{
			yyerror("Unimplemented feature: [pkg_body := PACKAGE BODY compound_name IS decl_part body_opt END c_id_opt ';']");
		}
	;

// ============================================================================================
// ============================================================================================

decl_part
	:	// empty!!
		{
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| decl_item_or_body_s1
		{
            $$ = DeclarationList::empty();
			yyerror("Unimplemented feature: [decl_part : decl_item_or_body_s1]");
		}
	;

// ============================================================================================
// ============================================================================================

decl_item_or_body_s1
	: decl_item_or_body
		{
			yyerror("Unimplemented feature: [decl_item_or_body_s1 := decl_item_or_body]");
		}

// --------------------------------------------------------------------------------------------

	| decl_item_or_body_s1 decl_item_or_body
		{
			yyerror("Unimplemented feature: [decl_item_or_body_s1 := decl_item_or_body_s1 decl_item_or_body]");
		}
	;

// ============================================================================================
// ============================================================================================

decl_item_or_body
	: body
		{
			yyerror("Unimplemented feature: [decl_item_or_body := body]");
		}

// --------------------------------------------------------------------------------------------

	| decl_item
		{
			yyerror("Unimplemented feature: [decl_item_or_body := decl_item]");
		}
	;

// ============================================================================================
// ============================================================================================

body
	: subprog_body
		{
			yyerror("Unimplemented feature: [body := subprog_body]");
//			$$ = new DeclarationList($1);;
		}

// --------------------------------------------------------------------------------------------

	| pkg_body
		{
			yyerror("Unimplemented feature: [body := pkg_body]");
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| task_body
		{
			yyerror("Unimplemented feature: [body := task_body]");
			$$ = DeclarationList::empty();
		}

// --------------------------------------------------------------------------------------------

	| prot_body
		{
			yyerror("Unimplemented feature: [body := prot_body]");
			$$ = DeclarationList::empty();
		}
	;

// ============================================================================================
// ============================================================================================

body_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: [body_opt := // empty!!]");
		}

// --------------------------------------------------------------------------------------------

	| block_body
		{
			yyerror("Unimplemented feature: [body_opt := block_body]");
		}
	;

// ============================================================================================
// ============================================================================================

block_body
	: BEGiN handled_stmt_s
		{
			$$ = $2;
		}
	;

// ============================================================================================
// ============================================================================================

handled_stmt_s
	: statement_s except_handler_part_opt
		{
			$$ = BlockBody::factory($1, $2);
		}
	;

// ============================================================================================
// ============================================================================================

except_handler_part_opt
	:	// empty!!
		{
			$$ = ExceptionList::empty();
		}

// --------------------------------------------------------------------------------------------

	| except_handler_part
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

except_handler_part
	: EXCEPTION exception_handler
		{
			yyerror("Unimplemented feature: [except_handler_part := EXCEPTION exception_handler]");
			$$ = ExceptionList::empty();
		}

// --------------------------------------------------------------------------------------------

	| except_handler_part exception_handler
		{
			yyerror("Unimplemented feature: [except_handler_part := except_handler_part exception_handler]");
			$$ = ExceptionList::empty();
		}
	;

// ============================================================================================
// ============================================================================================

exception_handler
	: WHEN except_choice_s RIGHT_SHAFT statement_s
		{
			yyerror("Unimplemented feature: [exception_handler := WHEN except_choice_s RIGHT_SHAFT statement_s]");
		}

// --------------------------------------------------------------------------------------------

	| WHEN identifier ':' except_choice_s RIGHT_SHAFT statement_s
		{
			yyerror("Unimplemented feature: [WHEN identifier ':' except_choice_s RIGHT_SHAFT statement_s]");
		}
	;

// ============================================================================================
// ============================================================================================

except_choice_s
	: except_choice
		{
			yyerror("Unimplemented feature: [except_choice_s := except_choice]");
		}

// --------------------------------------------------------------------------------------------

	| except_choice_s '|' except_choice
		{
			yyerror("Unimplemented feature: [except_choice_s := except_choice_s '|' except_choice]");
		}
	;

// ============================================================================================
// ============================================================================================

except_choice
	: name
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| OTHERS
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subprog_body
	: subprog_spec_is_push decl_part block_body END id_opt ';'
		{
			$$ = SubprogBody::factory($1, $2, $3, $5);
		}
	;

// ============================================================================================
// ============================================================================================

subprog_spec_is_push
	: subprog_spec IS
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

subprog_spec
	: PROCEDURE compound_name formal_part_opt
		{
			$$ = ProcedureSpec::factory($2, $3);
		}

// --------------------------------------------------------------------------------------------

	| FUNCTION designator formal_part_opt RETURN name
		{
			yyerror("Unimplemented feature: [subprog_spec := FUNCTION designator formal_part_opt RETURN name]");
		}

// --------------------------------------------------------------------------------------------

	| FUNCTION designator  // for generic inst and generic rename
		{
			yyerror("Unimplemented feature: [subprog_spec := FUNCTION designator]");
		}
	;

// ============================================================================================
// ============================================================================================

designator
	: compound_name
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| char_string
		{
            $$ = (StrLiteral *)$1;
		}
	;

// ============================================================================================
// ============================================================================================

formal_part_opt
	:	// empty!!
		{
			$$ = FormalParms::empty();
		}

// --------------------------------------------------------------------------------------------

	| formal_part
		{
			yyerror("Unimplemented feature: [formal_part_opt := formal_part]");
		}
	;

// ============================================================================================
// ============================================================================================

formal_part
	: '(' param_s ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

param_s
	: param
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| param_s ';' param
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

def_id_s
	: def_id
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| def_id_s ',' def_id
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

def_id
	: identifier
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

param
	: def_id_s ':' mode mark init_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| error
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

mode
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IN
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| OUT

		{
			yyerror("Unimplemented feature: []");
		}
// --------------------------------------------------------------------------------------------

	| IN OUT
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

mark
	: simple_name
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| mark TIC attribute_id
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| mark '.' simple_name
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

attribute_id
	: identifier
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DIGITS
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DELTA
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

init_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IS_ASSIGNED expression
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subunit
	: SEPARATE '(' compound_name ')' subunit_body
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subunit_body
	: subprog_body
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| pkg_body
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| task_body
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| prot_body
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

rename_unit
	: PACKAGE compound_name renames ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_spec renames ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_formal_part PACKAGE compound_name renames ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_formal_part subprog_spec renames ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

renames
	: RENAMES name
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_decl
	: generic_formal_part subprog_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_formal_part pkg_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_formal_part
	: GENERIC
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_formal_part generic_formal
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_formal
	: param ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| TYPE simple_name generic_discrim_part_opt IS generic_type_def ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WITH PROCEDURE simple_name formal_part_opt subp_default ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WITH FUNCTION designator formal_part_opt RETURN name subp_default ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WITH PACKAGE simple_name IS NEW name '(' BOX ')' ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WITH PACKAGE simple_name IS NEW name ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| use_clause
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_discrim_part_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| discrim_part
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| '(' BOX ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subp_default
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IS name
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IS BOX
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_type_def
	: '(' BOX ')'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| RANGE BOX
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| MOD BOX
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DELTA BOX
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DELTA BOX DIGITS BOX
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DIGITS BOX
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| array_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| access_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| private_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_derived_type
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_derived_type
	: NEW subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NEW subtype_ind WITH PRIVATE
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ABSTRACT NEW subtype_ind WITH PRIVATE
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

task_body
	: TASK BODY simple_name IS decl_part block_body END id_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_body
	: PROTECTED BODY simple_name IS prot_op_body_s END id_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

name
	: simple_name
		{
			$$ = IdentExpr::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| indexed_comp
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| selected_comp
		{
			yyerror("Unimplemented feature: [name := selected_comp]");
		}

// --------------------------------------------------------------------------------------------

	| attribute
		{
			yyerror("Unimplemented feature: [name := attribute]");
		}

// --------------------------------------------------------------------------------------------

	| operator_symbol
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

operator_symbol
	: char_string
		{
			$$ = StrExpr::factory(StrLiteral::factory($1));
		}
	;

// ============================================================================================
// ============================================================================================

indexed_comp
	: name '(' value_s ')'
		{
			$$ = ArrayOrCall::factory($1, $3);
		}
	;

// ============================================================================================
// ============================================================================================

value_s
	: value
		{
			$$ = ExpressionList::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| value_s ',' value
		{
			$$ = append($1, ExpressionList::factory($3));
		}
	;

// ============================================================================================
// ============================================================================================

value
	: expression
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| comp_assoc
		{
			yyerror("Unimplemented feature: [value := comp_assoc]");
		}

// --------------------------------------------------------------------------------------------

	| discrete_with_range
		{
			yyerror("Unimplemented feature: [value := discrete_with_range]");
		}

// --------------------------------------------------------------------------------------------

	| error
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

selected_comp
	: name '.' simple_name
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name '.' used_char
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name '.' operator_symbol
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name '.' ALL
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

attribute
	: name TIC attribute_id
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

used_char
	: char_lit
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

comp_assoc
	: choice_s RIGHT_SHAFT expression
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

discrete_with_range
	: name range_constraint
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| range
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

choice_s
	: choice
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| choice_s '|' choice
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

choice
	: expression
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| discrete_with_range
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| OTHERS
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

range_constraint
	: RANGE range
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

range
	: simple_expression DOT_DOT simple_expression
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name TIC RANGE
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name TIC RANGE '(' expression ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_op_body_s
	: pragma_s
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| prot_op_body_s prot_op_body pragma_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_op_body
	: entry_body
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_body
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

entry_body
	: ENTRY identifier formal_part_opt WHEN condition entry_body_part
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ENTRY identifier '(' iter_part discrete_range ')' formal_part_opt WHEN condition entry_body_part
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

entry_body_part
	: ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IS decl_part block_body END id_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

generic_subp_inst
	: subprog_spec IS generic_inst
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

block
	: label_opt block_decl block_body END id_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

decl
	: object_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| number_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| type_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subtype_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| pkg_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| task_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| prot_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| exception_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| rename_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| body_stub
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| error ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

object_decl
	: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

object_qualifier_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ALIASED
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| CONSTANT
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ALIASED CONSTANT
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

object_subtype_def
	: subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| array_type
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

number_decl
	: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

type_decl
	: TYPE identifier discrim_part_opt type_completion ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

discrim_part_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| discrim_part
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| '(' BOX ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

type_completion
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IS type_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

type_def
	: enumeration_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| integer_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| real_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| array_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| record_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| access_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| derived_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| private_type
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subtype_decl
	: SUBTYPE identifier IS subtype_ind ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subtype_ind
	: name constraint
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

constraint
	: range_constraint
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| decimal_digits_constraint
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

decimal_digits_constraint
	: DIGITS expression range_constr_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

derived_type
	: NEW subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NEW subtype_ind WITH PRIVATE
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NEW subtype_ind WITH record_def
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ABSTRACT NEW subtype_ind WITH PRIVATE
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ABSTRACT NEW subtype_ind WITH record_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

enumeration_type
	: '(' enum_id_s ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

enum_id_s
	: enum_id
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| enum_id_s ',' enum_id
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

enum_id
	: identifier
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| char_lit
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

integer_type
	: range_spec
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| MOD expression
		{
			yyerror("Unimplemented feature: []");
		}
	;


// ============================================================================================
// ============================================================================================

range_spec
	: range_constraint
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

range_spec_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| range_spec
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

real_type
	: float_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| fixed_type
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

float_type
	: DIGITS expression range_spec_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

fixed_type
	: DELTA expression range_spec
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DELTA expression DIGITS expression range_spec_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

array_type
	: unconstr_array_type
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| constr_array_type
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

unconstr_array_type
	: ARRAY '(' index_s ')' OF component_subtype_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

constr_array_type
	: ARRAY iter_index_constraint OF component_subtype_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

component_subtype_def
	: aliased_opt subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

aliased_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ALIASED
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

index_s
	: index
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| index_s ',' index
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

index
	: name RANGE BOX
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

iter_index_constraint
	: '(' iter_discrete_range_s ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

iter_discrete_range_s
	: discrete_range
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| iter_discrete_range_s ',' discrete_range
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

discrete_range
	: name range_constr_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| range
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

range_constr_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| range_constraint
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

record_type
	: tagged_opt limited_opt record_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

record_def
	: RECORD pragma_s comp_list END RECORD
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NuLL RECORD
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

tagged_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| TAGGED
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ABSTRACT TAGGED
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

comp_list
	: comp_decl_s variant_part_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| variant_part pragma_s
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NuLL ';' pragma_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

comp_decl_s
	: comp_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| comp_decl_s pragma_s comp_decl
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

variant_part_opt
	: pragma_s
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| pragma_s variant_part pragma_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

comp_decl
	: def_id_s ':' component_subtype_def init_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| error ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

discrim_part
	: '(' discrim_spec_s ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

discrim_spec_s
	: discrim_spec
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| discrim_spec_s ';' discrim_spec
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

discrim_spec
	: def_id_s ':' access_opt mark init_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| error
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

access_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

variant_part
	: CASE simple_name IS pragma_s variant_s END CASE ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

variant_s
	: variant
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| variant_s variant
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

variant
	: WHEN choice_s RIGHT_SHAFT pragma_s comp_list
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

access_type
	: ACCESS subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS CONSTANT subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS ALL subtype_ind
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS prot_opt PROCEDURE formal_part_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ACCESS prot_opt FUNCTION formal_part_opt RETURN mark
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| PROTECTED
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

literal
	: numeric_lit
		{
			yyerror("Unimplemented feature: []");
//			$$ = new NumericLiteral($1);
		}

// --------------------------------------------------------------------------------------------

	| used_char
		{
			yyerror("Unimplemented feature: []");
//			$$ = new CharLiteral($1);
		}

// --------------------------------------------------------------------------------------------

	| NuLL
		{
			yyerror("Unimplemented feature: []");
//			$$ = new NullLiteral();
		}
	;

// ============================================================================================
// ============================================================================================

aggregate
	: '(' comp_assoc ')'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| '(' value_s_2 ')'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| '(' expression WITH value_s ')'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| '(' expression WITH NuLL RECORD ')'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| '(' NuLL RECORD ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

value_s_2
	: value ',' value
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| value_s_2 ',' value
		{
			yyerror("Unimplemented feature: []");
		}
	;


// ============================================================================================
// ============================================================================================

expression
	: relation
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| expression logical relation
		{
			switch ($2) {
			case AND:
                yyerror("Unimplemented feature: []");
//				$$ = new AndExpr($1, $3);
//				break;

			case OR:
                yyerror("Unimplemented feature: []");
//				$$ = new OrExpr($1, $3);
//				break;

			case XOR:
                yyerror("Unimplemented feature: []");
//				$$ = new XorExpr($1, $3);
//				break;

			default:
				yyerror("Compiler error: Unknown logical operation");
				$$ = Expression::empty();
				break;
			}
		}

// --------------------------------------------------------------------------------------------

	| expression short_circuit relation
		{
			yyerror("Unimplemented feature: [expression := expression short_circuit relation]");
		}
	;

// ============================================================================================
// ============================================================================================

logical
	: AND
		{
			$$ = AND;
		}

// --------------------------------------------------------------------------------------------

	| OR
		{
			$$ = OR;
		}

// --------------------------------------------------------------------------------------------

	| XOR
		{
			$$ = XOR;
		}
	;

// ============================================================================================
// ============================================================================================

short_circuit
	: AND THEN
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| OR ELSE
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

relation
	: simple_expression
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| simple_expression relational simple_expression
		{
			switch ($2) {
			case EQ:
                yyerror("Unimplemented feature: []");
//				$$ = new EQExpr($1, $3);
//				break;

			case NE:
                yyerror("Unimplemented feature: []");
//				$$ = new NEExpr($1, $3);
//				break;

			case LT:
                yyerror("Unimplemented feature: []");
//				$$ = new LTExpr($1, $3);
//				break;

			case LT_EQ:
                yyerror("Unimplemented feature: []");
//				$$ = new LEExpr($1, $3);
//				break;

			case GT:
                yyerror("Unimplemented feature: []");
//				$$ = new GTExpr($1, $3);
//				break;

			case GE:
                yyerror("Unimplemented feature: []");
//				$$ = new GEExpr($1, $3);
//				break;

			default:
				yyerror("Compiler error: Unknown relational operation");
				$$ = Expression::empty();
				break;
			};
		}

// --------------------------------------------------------------------------------------------

	| simple_expression membership range
		{
			yyerror("Unimplemented feature: [relation := simple_expression membership range]");
		}

// --------------------------------------------------------------------------------------------

	| simple_expression membership name
		{
			yyerror("Unimplemented feature: [relation := simple_expression membership name]");
		}
	;

// ============================================================================================
// ============================================================================================

relational
	: '='
		{
			$$ = EQ;
		}

// --------------------------------------------------------------------------------------------

	| NE
		{
			$$ = NE;
		}

// --------------------------------------------------------------------------------------------

	| '<'
		{
			$$ = LT;
		}

// --------------------------------------------------------------------------------------------

	| LT_EQ
		{
			$$ = LT_EQ;
		}

// --------------------------------------------------------------------------------------------

	| '>'
		{
			$$ = GT;
		}

// --------------------------------------------------------------------------------------------

	| GE
		{
			$$ = GE;
		}
	;

// ============================================================================================
// ============================================================================================

membership
	: IN
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NOT IN
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

simple_expression
	: unary term
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| term
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| simple_expression adding term
		{
			switch ($2) {
			case PLUS:
                yyerror("Unimplemented feature: []");
//				$$ = new PlusExpr($1, $3);
//				break;

			case MINUS:
                yyerror("Unimplemented feature: []");
//				$$ = new MinusExpr($1, $3);
//				break;

			default:
				yyerror("Compiler error: Unknown adding operation");
				$$ = Expression::empty();
				break;
			}
		}
	;

// ============================================================================================
// ============================================================================================

unary
	: '+'
		{
			$$ = PLUS;
		}

// --------------------------------------------------------------------------------------------

	| '-'
		{
			$$ = MINUS;
		}
	;

// ============================================================================================
// ============================================================================================

adding
	: '+'
		{
			$$ = PLUS;
		}

// --------------------------------------------------------------------------------------------

	| '-'
		{
			$$ = MINUS;
		}

// --------------------------------------------------------------------------------------------

	| '&'
		{
			yyerror("Unimplemented feature: [adding := '&']");
		}
	;

// ============================================================================================
// ============================================================================================

term
	: factor
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| term multiplying factor
		{
			switch ($2) {
			case MUL:
                yyerror("Unimplemented feature: []");
//				$$ = new MulExpr($1, $3);
//				break;

			case DIV:
                yyerror("Unimplemented feature: []");
//				$$ = new DivExpr($1, $3);
//				break;

			case MOD:
                yyerror("Unimplemented feature: []");
//				$$ = new ModExpr($1, $3);
//				break;

			case REM:
                yyerror("Unimplemented feature: []");
//				$$ = new RemExpr($1, $3);
//				break;

			default:
				yyerror("Compiler error: Unknown multiplying operation");
				$$ = Expression::empty();
				break;
			}
		}
	;

// ============================================================================================
// ============================================================================================

multiplying
	: '*'
		{
			$$ = MUL;
		}

// --------------------------------------------------------------------------------------------

	| '/'
		{
			$$ = DIV;
		}

// --------------------------------------------------------------------------------------------

	| MOD
		{
			$$ = MOD;
		}

// --------------------------------------------------------------------------------------------

	| REM
		{
			$$ = REM;
		}
	;

// ============================================================================================
// ============================================================================================

factor
	: primary
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| NOT primary
		{
			yyerror("Unimplemented feature: []");
//			$$ = new NotExpr($2);
		}

// --------------------------------------------------------------------------------------------

	| ABS primary
		{
			yyerror("Unimplemented feature: []");
//			$$ = new AbsExpr($2);
		}

// --------------------------------------------------------------------------------------------

	| primary EXPON primary
		{
			yyerror("Unimplemented feature: []");
//			$$ = new ExpExpr($1, $3);
		}
	;

// ============================================================================================
// ============================================================================================

primary
	: literal
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| name
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| allocator
		{
			yyerror("Unimplemented feature: [primary := allocator]");
			$$ = Expression::empty();
		}

// --------------------------------------------------------------------------------------------

	| qualified
		{
			yyerror("Unimplemented feature: [primary := qualified]");
			$$ = Expression::empty();
		}

// --------------------------------------------------------------------------------------------

	| parenthesized_primary
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

parenthesized_primary
	: aggregate
		{
			yyerror("Unimplemented feature: [parenthesized_primary := aggregate]");
		}

// --------------------------------------------------------------------------------------------

	| '(' expression ')'
		{
			$$ = $2;
		}
	;

// ============================================================================================
// ============================================================================================

qualified
	: name TIC parenthesized_primary
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

allocator
	: NEW name
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| NEW qualified
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

statement_s
	: statement
		{
			$$ = StatementList::factory($1);
		}

// --------------------------------------------------------------------------------------------

	| statement_s statement
		{
			$$ = append($1, StatementList::factory($2));
		}
	;

// ============================================================================================
// ============================================================================================

statement
	: unlabeled
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| label statement
		{
			yyerror("Unimplemented feature: []");
//			$2->labels = append(new LabelList($1), $2->labels);
			$$ = $2;
		}
	;

// ============================================================================================
// ============================================================================================

unlabeled
	: simple_stmt
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| compound_stmt
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| pragma
		{
			yyerror("Unimplemented feature: [unlabeled := pragma]");
		}
	;

// ============================================================================================
// ============================================================================================

simple_stmt
	: null_stmt
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| assign_stmt
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| exit_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := exit_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| return_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := return_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| goto_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := goto_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| procedure_call
		{
			$$ = $1;
		}

// --------------------------------------------------------------------------------------------

	| delay_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := delay_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| abort_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := abort_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| raise_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := raise_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| code_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := code_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| requeue_stmt
		{
			yyerror("Unimplemented feature: [simple_stmt := requeue_stmt]");
			$$ = Statement::empty();
		}

// --------------------------------------------------------------------------------------------

	| error ';'
		{
			yyerror("Unimplemented feature: []");
			$$ = Statement::empty();
		}
	;

// ============================================================================================
// ============================================================================================

compound_stmt
	: if_stmt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| case_stmt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| loop_stmt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| block
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| accept_stmt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| select_stmt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

label
	: LT_LT identifier GT_GT
		{
			yyerror("Unimplemented feature: []");
//			$$ = new Label($2);
		}
	;

// ============================================================================================
// ============================================================================================

null_stmt
	: NuLL ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

assign_stmt
	: name IS_ASSIGNED expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

if_stmt :
	IF cond_clause_s else_opt END IF ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

cond_clause_s
	: cond_clause
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| cond_clause_s ELSIF cond_clause
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

cond_clause
	: cond_part statement_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

cond_part
	: condition THEN
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

condition
	: expression
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

else_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ELSE statement_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

case_stmt
	: case_hdr pragma_s alternative_s END CASE ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

case_hdr
	: CASE expression IS
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

alternative_s
	:
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| alternative_s alternative
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

alternative
	: WHEN choice_s RIGHT_SHAFT statement_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

loop_stmt
	: label_opt iteration basic_loop id_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

label_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| identifier ':'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

iteration
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WHILE condition
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| iter_part reverse_opt discrete_range
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

iter_part
	: FOR identifier IN
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

reverse_opt
	: // empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| REVERSE
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

basic_loop
	: LOOP statement_s END LOOP
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

id_opt
	:	// empty !!
		{
			$$ = Identifier::empty();
		}

// --------------------------------------------------------------------------------------------

	| designator
		{
			$$ = $1;
		}
	;

// ============================================================================================
// ============================================================================================

block_decl
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DECLARE decl_part
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

exit_stmt
	: EXIT name_opt when_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

name_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| name
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

when_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WHEN condition
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

return_stmt
	: RETURN ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| RETURN expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

goto_stmt
	: GOTO name ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

subprog_decl
	: subprog_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| generic_subp_inst ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_spec_is_push ABSTRACT ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

procedure_call
	: name ';'
		{
			$$ = ProcedureCall::factory($1);
		}
	;

// ============================================================================================
// ============================================================================================

private_type
	: tagged_opt limited_opt PRIVATE
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

limited_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| LIMITED
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

rename_decl
	: def_id_s ':' object_qualifier_opt subtype_ind renames ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| def_id_s ':' EXCEPTION renames ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| rename_unit
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

task_decl
	: task_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

task_spec
	: TASK simple_name task_def
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| TASK TYPE simple_name discrim_part_opt task_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

task_def
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| IS entry_decl_s rep_spec_s task_private_opt END id_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

task_private_opt
	:	// empty!!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| PRIVATE entry_decl_s rep_spec_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_decl
	: prot_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_spec
	: PROTECTED identifier prot_def
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| PROTECTED TYPE simple_name discrim_part_opt prot_def
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_def
	: IS prot_op_decl_s prot_private_opt END id_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_private_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| PRIVATE prot_elem_decl_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_op_decl_s
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| prot_op_decl_s prot_op_decl
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_op_decl
	: entry_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_spec ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| rep_spec
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| pragma
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_elem_decl_s
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| prot_elem_decl_s prot_elem_decl
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

prot_elem_decl
	: prot_op_decl
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| comp_decl
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

entry_decl_s
	: pragma_s
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| entry_decl_s entry_decl pragma_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

entry_decl
	: ENTRY identifier formal_part_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| ENTRY identifier '(' discrete_range ')' formal_part_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

rep_spec_s
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| rep_spec_s rep_spec pragma_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

entry_call
	: procedure_call
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

accept_stmt
	: accept_hdr ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| accept_hdr DO handled_stmt_s END id_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

accept_hdr
	: ACCEPT entry_name formal_part_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

entry_name
	: simple_name
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| entry_name '(' expression ')'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

delay_stmt
	: DELAY expression ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| DELAY UNTIL expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

select_stmt
	: select_wait
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| async_select
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| timed_entry_call
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| cond_entry_call
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

select_wait
	: SELECT guarded_select_alt or_select else_opt END SELECT ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

guarded_select_alt
	: select_alt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| WHEN condition RIGHT_SHAFT select_alt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

or_select
	:	// empty  !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| or_select OR guarded_select_alt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

select_alt
	: accept_stmt stmts_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| delay_stmt stmts_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| TERMINATE ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

delay_or_entry_alt
	: delay_stmt stmts_opt
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| entry_call stmts_opt
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

async_select
	: SELECT delay_or_entry_alt THEN ABORT statement_s END SELECT ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

timed_entry_call
	: SELECT entry_call stmts_opt OR delay_stmt stmts_opt END SELECT ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

cond_entry_call
	: SELECT entry_call stmts_opt ELSE statement_s END SELECT ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

stmts_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| statement_s
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

abort_stmt
	: ABORT name_s ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

body_stub
	: TASK BODY simple_name IS SEPARATE ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| PACKAGE BODY compound_name IS SEPARATE ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| subprog_spec IS SEPARATE ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| PROTECTED BODY simple_name IS SEPARATE ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

exception_decl
	: def_id_s ':' EXCEPTION ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

raise_stmt
	: RAISE name_opt ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

requeue_stmt
	: REQUEUE name ';'
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| REQUEUE name WITH ABORT ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

rep_spec
	: attrib_def
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| record_type_spec
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| address_spec
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

attrib_def
	: FOR mark USE expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

record_type_spec
	: FOR mark USE RECORD align_opt comp_loc_s END RECORD ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

align_opt
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| AT MOD expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

comp_loc_s
	:	// empty !!
		{
			yyerror("Unimplemented feature: []");
		}

// --------------------------------------------------------------------------------------------

	| comp_loc_s mark AT expression RANGE range ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

address_spec
	: FOR mark USE AT expression ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

code_stmt
	: qualified ';'
		{
			yyerror("Unimplemented feature: []");
		}
	;

// ============================================================================================
// ============================================================================================

%%

#include <iostream>
#include <cstring>

extern int openBuffer(const char *name);
string GlobalNodeFile;
string DumpToken(int token);
int curr_lineno = 1;
int parse_error = 0;
int semant_error = 0;
int mydebug = 1;
bool printTree = false;
int GlobalNodeLine = 0;
AdaPgm *ast;

int main(int argc, char *argv[])
{
	int token;
	int i;
	AdaPgm *wrk = 0;
	extern int yy_flex_debug;

	yydebug = 1;
	yy_flex_debug = 0;

	for (i = 1; i < argc; i ++) {
		if (strcmp(argv[i], "--debug") == 0) yydebug = 1;
		if (strcmp(argv[i], "--print") == 0) printTree = true;
	}

	for (i = 1; i < argc; i ++) {
		if (openBuffer(argv[i])) {
			if (mydebug) {
				cout << "\nParsing " << argv[i] << endl;
				cout << "-----------------------------" << endl;
			}

			if (yyparse()) cerr << "ERROR!!" << endl;
			wrk = append(wrk, ast);
		}
	}

	ast = wrk;

	if (!ast) {
		cerr << "COMPILER ERROR: Abstract Syntax Tree is NULL" << endl;
		return 1;
	}

	if (parse_error) {
		cerr << parse_error << " parse errors detected; exiting" << endl;
		return 1;
	}

	if (printTree) {
		cout << "Parse complete; starting Semantic Analysis" << endl;
		cout << "------------------------------------------" << endl;
		ast->Print(cout, 1);
	}

//	Semant(ast, tcEnv, "");
//	cout << "Semant complete\n";
//	tcEnv->Print();

	if (semant_error) {
		cerr << semant_error << " semantic errors detected; exiting\n";
		return 1;
	}

    return 0;
}
