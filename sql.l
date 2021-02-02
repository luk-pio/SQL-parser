%option noyywrap yylineno case-insensitive

%{
  #include "sql.tab.h"
  #include <stdarg.h>
  #include <string.h>

  void yyerror(char *s, ...);

  int oldstate;
%}

%x COMMENT
%s BTWMODE


%%

    /* SELECT */

FROM {return FROM;}
SELECT {return SELECT;}

    /* SELECT options */

ALL     { return ALL; }
DISTINCT     { return DISTINCT; }
DISTINCTROW     { return DISTINCTROW; }
STRAIGHT_JOIN     { return STRAIGHT_JOIN; }

WHERE     { return WHERE; }
GROUP     { return GROUP; }
BY     { return BY; }
AS     { return AS; }
HAVING     { return HAVING; }
ORDER     { return ORDER; }
LIMIT     { return LIMIT; }

ASC     { return ASC; }
DESC     { return DESC; }

ROLLUP     { return ROLLUP; }

INTO     { return LIMIT; }
JOIN     { return JOIN; }
ON     { return ON; }
INNER     { return INNER; }
CROSS     { return CROSS; }
RIGHT     { return RIGHT; }
LEFT     { return LEFT; }
USING     { return USING; }
FOR     { return FOR; }

    /* SELECT expression */

  /* Hack for BETWEEN ... AND ...
   * return special AND token if BETWEEN seen
   */
<BTWMODE>AND    { BEGIN INITIAL; return AND; }
AND     { return ANDOP; }

    /* expressions */

    /* punctuation and operators */

[-+&~|^/%*(),.;!]   { return yytext[0]; }

"&&"            { return ANDOP; }
"||"            { return OR; }

"="     { yylval.strval = strdup(yytext);
                          return COMPARISON; }
"<=>"   { yylval.strval = strdup(yytext);
                          return COMPARISON; }
">="    { yylval.strval = strdup(yytext);
                          return COMPARISON; }
">"     { yylval.strval = strdup(yytext);
                          return COMPARISON; }
"<="    { yylval.strval = strdup(yytext);
                          return COMPARISON; }
"<"     { yylval.strval = strdup(yytext);
                          return COMPARISON; }
"!="    |
"<>"    { yylval.strval = strdup(yytext);
                          return COMPARISON; }

":="     { return ASSIGN; }

MOD { return MOD; }
XOR { return XOR; }
NOT { return NOT; }
SOME { return SOME; }
ANY { return ANY; }
IS { return IS; }
NULL { return NULLX; }

    /* Integers */

-?[0-9]+ { yylval.intval = atoi (yytext);
         return INTNUM; }

    /* Floating point */

-?[0-9]+"."[0-9]* |
-?"."[0-9]+     |
-?[0-9]+E[-+]?[0-9]+    |
-?[0-9]+"."[0-9]*E[-+]?[0-9]+ |
-?"."[0-9]+E[-+]?[0-9]+ { yylval.floatval = atof(yytext) ;
                                  return APPROXNUM; }

   /* strings */

'(\\.|''|[^'\n])*'   |
\"(\\.|\"\"|[^"\n])*\"  { yylval.strval = strdup(yytext); return STRING; }

'(\\.|[^'\n])*$      { yyerror("Unterminated string %s", yytext); }
\"(\\.|[^"\n])*$    { yyerror("Unterminated string %s", yytext); }


    /* booleans */

TRUE    { yylval.intval = 1; return BOOL; }
UNKNOWN { yylval.intval = -1; return BOOL; }
FALSE   { yylval.intval = 0; return BOOL; }

    /* Variable names */

[A-Za-z][A-Za-z0-9_]*   { yylval.strval = strdup(yytext);
                          return NAME; }

`[^`/\\.\n]+`           { yylval.strval = strdup(yytext+1);
                          yylval.strval[yyleng-2] = 0;
                          return NAME; }
@[0-9a-z_.$]+ |
@\"[^"\n]+\" |
@`[^`\n]+` |
@'[^'\n]+' { yylval.strval = strdup(yytext+1); return USERVAR; }

    /* whitespace */

[ \t\n]

    /* etc */
.               { yyerror("mystery character '%c'", *yytext); }

%%
