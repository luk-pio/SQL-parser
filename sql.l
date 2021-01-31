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

FROM {return FROM;}
SELECT {return SELECT;}


[-+&~|^/%*(),.;!]   { return yytext[0]; }

-?[0-9]+ { yylval.intval = atoi (yytext);
         return INTNUM; }

[A-Za-z][A-Za-z0-9_]*   { yylval.strval = strdup(yytext);
                          return NAME; }

`[^`/\\.\n]+`           { yylval.strval = strdup(yytext+1);
                          yylval.strval[yyleng-2] = 0;
                          return NAME; }

@[0-9a-z_.$]+ |
@\"[^"\n]+\" |
@`[^`\n]+` |
@'[^'\n]+' { yylval.strval = strdup(yytext+1); return USERVAR; }

[ \t\n]
.               { yyerror("mystery character '%c'", *yytext); }

%%
