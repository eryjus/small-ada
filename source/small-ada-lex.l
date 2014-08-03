%{
    #include "StringTables.h"
    #include "small-ada-tree.h"
    #include "small-ada-parse.h"
	extern int curr_lineno;

	#define MAX_STR		4096

	char string_buf[MAX_STR + 1];
	char *string_ptr = 0;
%}

WS							[ \t]

COMMENT						--
LETTER						[A-Za-z]
DIGIT						[0-9]
HEX							[0-9a-fA-F]
LET_DIG						[A-Za-z0-9]
NUMERAL						[0-9](_?[0-9])*
BASE_NUMERAL				[0-9a-fA-F](_?[0-9a-fA-F])*
BASE						0*(2|3|4|5|6|7|8|9|10|11|12|13|14|15|16)
E							[eE]

%x							STR
%x							STR_SKIP
%x							_WITH_
%x                          W_SKIP

%%

{WS}						{}
{COMMENT}.*					{}

\n							{   curr_lineno ++; }

(?i:ABORT)					{	return (ABORT); }
(?i:ABS)					{	return (ABS); }
(?i:ABSTRACT)				{	return (ABSTRACT); }
(?i:ACCEPT)					{	return (ACCEPT); }
(?i:ACCESS)					{	return (ACCESS); }
(?i:ALIASED)				{	return (ALIASED); }
(?i:ALL)					{	return (ALL); }
(?i:AND)					{	return (AND); }
(?i:ARRAY)					{	return (ARRAY); }
(?i:AT)						{	return (AT); }
(?i:BEGIN)					{	return (BEGiN); }
(?i:BODY)					{	return (BODY); }
(?i:CASE)					{	return (CASE); }
(?i:CONSTANT)				{	return (CONSTANT); }
(?i:DECLARE)				{	return (DECLARE); }
(?i:DELAY)					{	return (DELAY); }
(?i:DELTA)					{	return (DELTA); }
(?i:DIGITS)					{	return (DIGITS); }
(?i:DO)						{	return (DO); }
(?i:ELSE)					{	return (ELSE); }
(?i:ELSIF)					{	return (ELSIF); }
(?i:END)					{	return (END); }
(?i:ENTRY)					{	return (ENTRY); }
(?i:EXCEPTION)				{	return (EXCEPTION); }
(?i:EXIT)					{	return (EXIT); }
(?i:FOR)					{	return (FOR); }
(?i:FUNCTION)				{	return (FUNCTION); }
(?i:GENERIC)				{	return (GENERIC); }
(?i:GOTO)					{	return (GOTO); }
(?i:IF)						{	return (IF); }
(?i:IN)						{	return (IN); }
(?i:IS)						{	return (IS); }
(?i:LIMITED)				{	return (LIMITED); }
(?i:LOOP)					{	return (LOOP); }
(?i:MOD)					{	return (MOD); }
(?i:NEW)					{	return (NEW); }
(?i:NOT)					{	return (NOT); }
(?i:NULL)					{	return (NuLL); }
(?i:OF)						{	return (OF); }
(?i:OR)						{	return (OR); }
(?i:OTHERS)					{	return (OTHERS); }
(?i:OUT)					{	return (OUT); }
(?i:PACKAGE)				{	return (PACKAGE); }
(?i:PRAGMA)					{	return (PRAGMA); }
(?i:PRIVATE)				{	return (PRIVATE); }
(?i:PROCEDURE)				{	return (PROCEDURE); }
(?i:PROTECTED)				{	return (PROTECTED); }
(?i:RAISE)					{	return (RAISE); }
(?i:RANGE)					{	return (RANGE); }
(?i:RECORD)					{	return (RECORD); }
(?i:REM)					{	return (REM); }
(?i:RENAMES)				{	return (RENAMES); }
(?i:REQUEUE)				{	return (REQUEUE); }
(?i:RETURN)					{	return (RETURN); }
(?i:REVERSE)				{	return (REVERSE); }
(?i:SELECT)					{	return (SELECT); }
(?i:SEPARATE)				{	return (SEPARATE); }
(?i:SUBTYPE)				{	return (SUBTYPE); }
(?i:TAGGED)					{	return (TAGGED); }
(?i:TASK)					{	return (TASK); }
(?i:TERMINATE)				{	return (TERMINATE); }
(?i:THEN)					{	return (THEN); }
(?i:TYPE)					{	return (TYPE); }
(?i:UNTIL)					{	return (UNTIL); }
(?i:USE)					{	return (USE); }
(?i:WHEN)					{	return (WHEN); }
(?i:WHILE)					{	return (WHILE); }

(?i:WITH)					{   return (WITH); }

(?i:XOR)					{	return (XOR); }

[-&()*+,./;:<=>|]			{	return yytext[0]; }
=>							{	return RIGHT_SHAFT; }
\.\.						{	return DOT_DOT; }
\*\*						{	return EXPON; }
:=							{	return IS_ASSIGNED; }
\/=							{	return NE; }
>=							{	return GE; }
\<=							{	return LT_EQ; }
\<\<						{	return LT_LT; }
>>							{	return GT_GT; }
\<>							{	return BOX; }
\'                          {   return TIC; }

\'.\'						{	yylval.character = yytext[1];
								return (char_lit);
							}

\"							{	string_ptr = string_buf;
								BEGIN(STR);
							}
<STR>\"						{	*string_ptr = 0;
								BEGIN(INITIAL);
								yylval.str = strTable.AddString(string_buf);
								return (char_string);
							}
<STR>\n						{	yylval.error_msg = "Unterminated string";
								BEGIN(INITIAL);
								return (ERROR);
							}
<STR>\0						{	yylval.error_msg = "NULL embedded in string literal";
								BEGIN(STR_SKIP);
							}
<STR><<EOF>>				{	yylval.error_msg = "EOF detected in string literal";
								BEGIN(INITIAL);
								return (ERROR);
							}
<STR>\"\"					{	*string_ptr ++ = '\"';
								if (string_ptr - string_buf > MAX_STR) {
									yylval.error_msg = "String literal too long";
									BEGIN(STR_SKIP);
								}
							}
<STR>.						{	*string_ptr ++ = yytext[0];
								if (string_ptr - string_buf > MAX_STR) {
									yylval.error_msg = "String literal too long";
									BEGIN(STR_SKIP);
								}
							}
<STR_SKIP>[^\n\"]*			{}
<STR_SKIP>[\n\"]			{	BEGIN(INITIAL);
								return (ERROR);
							}

(({LETTER}({LET_DIG}|_{LET_DIG})*{LET_DIG})|{LETTER})		{
								yylval.id = idTable.AddString(yytext);
								return (identifier);
							}
{BASE}"#"{BASE_NUMERAL}(\.{BASE_NUMERAL})?"#"((({E}\+?{NUMERAL})|({E}-{NUMERAL})))?	{
								yylval.nbr = numTable.AddString(yytext);
								return (numeric_lit);
							}
{NUMERAL}(\.{NUMERAL})?(({E}\+?{NUMERAL})|({E}-{NUMERAL}))?		{
								yylval.nbr = numTable.AddString(yytext);
								return (numeric_lit);
							}

<<EOF>>						{
                                if(yydebug) cout << "<<EOF>> reached; closing.\n";
                                yypop_buffer_state();

                                if (!YY_CURRENT_BUFFER) yyterminate();
							}

 /* ====================================================================================================== */

{LETTER}(_|{LET_DIG})*		{	yylval.error_msg = "Illegally formed identifier";
								return (ERROR);
							}

{NUMERAL}"#"{BASE_NUMERAL}(\.{BASE_NUMERAL})?"#"((({E}\+?{NUMERAL})|(E-{NUMERAL})))?	{
								yylval.error_msg = "Illegally formed number";
								return (ERROR);
							}

.							{	yylval.error_msg = "Invalid character";
								return (ERROR);
							}

%%

#include <iostream>

int yywrap(void) { return 1; }

int openBuffer(const char *name)
{
    extern bool mydebug;

    if (yyin) fclose(yyin);
	yyin = fopen(name, "r");

	if (!yyin) return 0;

    yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
//	yy_switch_to_buffer(yy_create_buffer(yyin, YY_BUF_SIZE));
	curr_lineno = 1;
    GlobalNodeFile = name;

    if(yydebug) cout << name << " is now open.\n";

	return 1;
}

string dig(int i)
{
	char buf[25];

	sprintf(buf, "%d", i);
	return string(buf);
}

string dig0(int i, int l)
{
	char buf[25];

	sprintf(buf, "%*d", l, i);
	return string(buf);
}

string DumpToken(int token)
{
	string tok = "#" + dig(curr_lineno) + " ";

	switch (token) {
	case TIC:
		tok += "'";
		break;

    case DOT_DOT:
		tok += "..";
		break;

    case LT_LT:
		tok += "<<";
		break;

    case BOX:
		tok += "<>";
		break;

    case LT_EQ:
		tok += "<=";
		break;

    case EXPON:
		tok += "**";
		break;

    case NE:
		tok += "/=";
		break;

    case GT_GT:
		tok += ">>";
		break;

    case GE:
		tok += ">=";
		break;

    case IS_ASSIGNED:
		tok += ":=";
		break;

    case RIGHT_SHAFT:
		tok += "=>";
		break;

    case ABORT:
		tok += "ABORT";
		break;

    case ABS:
		tok += "ABS";
		break;

    case ABSTRACT:
		tok += "ABSTRACT";
		break;

    case ACCEPT:
		tok += "ACCEPT";
		break;

    case ACCESS:
		tok += "ACCESS";
		break;

    case ALIASED:
		tok += "ALIASED";
		break;

    case ALL:
		tok += "ALL";
		break;

    case AND:
		tok += "AND";
		break;

    case ARRAY:
		tok += "ARRAY";
		break;

    case AT:
		tok += "AT";
		break;

    case BEGiN:
		tok += "BEGIN";
		break;

    case BODY:
		tok += "BODY";
		break;

    case CASE:
		tok += "CASE";
		break;

    case CONSTANT:
		tok += "CONSTANT";
		break;

    case DECLARE:
		tok += "DECLARE";
		break;

    case DELAY:
		tok += "DELAY";
		break;

    case DELTA:
		tok += "DELTA";
		break;

    case DIGITS:
		tok += "DIGITS";
		break;

    case DO:
		tok += "DO";
		break;

    case ELSE:
		tok += "ELSE";
		break;

    case ELSIF:
		tok += "ELSIF";
		break;

    case END:
		tok += "END";
		break;

    case ENTRY:
		tok += "ENTRY";
		break;

    case EXCEPTION:
		tok += "EXCEPTION";
		break;

    case EXIT:
		tok += "EXIT";
		break;

    case FOR:
		tok += "FOR";
		break;

    case FUNCTION:
		tok += "FUNCTION";
		break;

    case GENERIC:
		tok += "GENERIC";
		break;

    case GOTO:
		tok += "GOTO";
		break;

    case IF:
		tok += "IF";
		break;

    case IN:
		tok += "IN";
		break;

    case IS:
		tok += "IS:";
		break;

    case LIMITED:
		tok += "LIMITED";
		break;

    case LOOP:
		tok += "LOOP";
		break;

    case MOD:
		tok += "MOD";
		break;

    case NEW:
		tok += "NEW";
		break;

    case NOT:
		tok += "NOT";
		break;

    case NuLL:
		tok += "NULL";
		break;

    case OF:
		tok += "OF";
		break;

    case OR:
		tok += "OR";
		break;

    case OTHERS:
		tok += "OTHERS";
		break;

    case OUT:
		tok += "OUT";
		break;

    case PACKAGE:
		tok += "PACKAGE";
		break;

    case PRAGMA:
		tok += "PRAGMA";
		break;

    case PRIVATE:
		tok += "PRIVATE";
		break;

    case PROCEDURE:
		tok += "PROCEDURE";
		break;

    case PROTECTED:
		tok += "PROTECTED";
		break;

    case RAISE:
		tok += "RAISE";
		break;

    case RANGE:
		tok += "RANGE";
		break;

    case RECORD:
		tok += "RECORD";
		break;

    case REM:
		tok += "REM";
		break;

    case RENAMES:
		tok += "RENAMES";
		break;

    case REQUEUE:
		tok += "REQUEUE";
		break;

    case RETURN:
		tok += "RETURN";
		break;

    case REVERSE:
		tok += "REVERSE";
		break;

    case SELECT:
		tok += "SELECT";
		break;

    case SEPARATE:
		tok += "SEPARATE";
		break;

    case SUBTYPE:
		tok += "SUBTYPE";
		break;

    case TAGGED:
		tok += "TAGGED";
		break;

    case TASK:
		tok += "TASK";
		break;

    case TERMINATE:
		tok += "TERMINATE";
		break;

    case THEN:
		tok += "THEN";
		break;

    case TYPE:
		tok += "TYPE";
		break;

    case UNTIL:
		tok += "UNTIL";
		break;

    case USE:
		tok += "USE";
		break;

    case WHEN:
		tok += "WHEN";
		break;

    case WHILE:
		tok += "WHILE";
		break;

    case WITH:
		tok += "WITH";
		break;

    case XOR:
		tok += "XOR";
		break;

    case char_lit:
		tok += "Character Literal: '";
		tok += yylval.character;
		tok += "'";
		break;

    case identifier:
		tok += "Identifier: ";
		tok += yylval.id->GetKeyValue();
		break;

    case char_string:
		tok += "String Literal: \"";
		tok += yylval.str->GetKeyValue();
		tok += "\"";
		break;

    case numeric_lit:
		tok += "Numeric Literal: #";
		tok += yylval.nbr->GetKeyValue();
		break;

	default:
		if (token < 127) tok += (char)token;
		else {
			tok += "Unknown token type: ";
			tok += dig(token);
		}

		break;
	}

	return tok;
}
