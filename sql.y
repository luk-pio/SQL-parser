%{
    #include <stdlib.h>
    #include <stdarg.h>
    #include <stddef.h>
    #include <stdio.h>
    #include <string.h>
    #include <math.h>
    #include <sys/queue.h>

    #define BUFF_SIZE 1024

    int yydebug=0;
    int print_rpn=0;

    int yylex(void);
    void rpn(char *s, ...);
    void stack_push(char *s, int numargs, int arg_reps[]);
    void stack_pushf(char *s, int numargs, int arg_reps[], int maxsize, ...);
    char *ftostr(float i);
    char *itostr(int i);
    char *btostr(int b);
    int yyerror(const char *s, ...);

    struct stack_entry {
      char *fstring;
      int numargs;
      int arg_reps[BUFF_SIZE];
      SLIST_ENTRY(stack_entry) entries;
    };

    SLIST_HEAD(listhead, stack_entry);
    struct listhead stack_head;

%}

%union {
        int intval;
        double floatval;
        char *strval;
        int subtok;
}

       /* names and literals */

%token <strval> NAME
%token <strval> STRING
%token <intval> INTNUM
%token <intval> BOOL
%token <floatval> APPROXNUM

%token <strval> USERVAR

       /* Keywords */

/* SELECT */
%token FROM
%token SELECT
%token ALL
%token DISTINCT
%token DISTINCTROW
%token STRAIGHT_JOIN

%token WHERE
%token GROUP
%token BY
%token AS
%token HAVING
%token ORDER
%token LIMIT
%token ROLLUP
%token INTO
%token ON
%token JOIN
%token WITH
%token NATURAL
%token INNER
%token CROSS
%token OUTER
%token LEFT
%token RIGHT
%token USING
%token KEY
%token IGNORE
%token FORCE
%token FOR
%token USE


%token ASC
%token DESC
/* expressions */

%token ANY
%token SOME
%token NULLX

       /* operators and precedence levels */

%right ASSIGN
%left OR XOR ANDOP AND
%nonassoc IN IS LIKE REGEXP
%left NOT '!'
%left BETWEEN
%left <strval> COMPARISON /* = <> < > <= >= <=> */
%left '|'
%left '&'
%left <subtok> SHIFT /* << >> */
%left '+' '-'
%left '*' '/' '%' MOD
%left '^'
%nonassoc UMINUS


       /* Non-terminals */

%type <intval> expr
%type <intval> select_opts select_expr_list
%type <intval> groupby_list opt_with_rollup opt_asc_desc
%type <intval> table_references opt_inner_cross opt_outer
%type <intval> left_or_right opt_left_or_right_outer column_list

%type <intval> stmt_list select_stmt
%type <intval> stmt

%%

// TOP
stmt_list: stmt ';' { $$ = 1; } 
| stmt_list stmt ';' { $$ = $1 + 1; rpn("STMTLIST %i %i", $1, 1); int arg_reps[] = {1 + 1}; stack_push("{}\n{}", 2, arg_reps); }
;

stmt: select_stmt { $$ = $1; rpn("STMT"); int arg_reps[] = {$1}; stack_push("{};", 1, arg_reps); };

expr: NAME         { $$ = 1; rpn("NAME %s", $1); int arg_reps[] = {}; stack_push($1, 0, arg_reps); free($1);}
   | NAME '.' NAME { $$ = 1; rpn("FIELDNAME %s.%s", $1, $3); int arg_reps[] = {}; stack_pushf("%s.%s", 0, arg_reps, BUFF_SIZE, $1, $3); free($1); free($3); }
   | USERVAR       { $$ = 1; rpn("USERVAR %s", $1); int arg_reps[] = {}; stack_push($1, 0, arg_reps); free($1); }
   | STRING        { $$ = 1; rpn("STRING %s", $1); int arg_reps[] = {}; stack_pushf("%s", 0, arg_reps, BUFF_SIZE, $1); free($1); }
   | INTNUM        { $$ = 1; rpn("INTNUM %i", $1); int arg_reps[] = {}; stack_push(itostr($1), 0, arg_reps); }
   | APPROXNUM     { $$ = 1; rpn("FLOAT %g", $1); int arg_reps[] = {}; stack_push(ftostr($1), 0, arg_reps); }
   | BOOL          { $$ = 1; rpn("BOOL %d", $1); int arg_reps[] = {}; stack_push(btostr($1), 0, arg_reps); }
   ;

expr: expr '+' expr { rpn("ADD"); int arg_reps[] = {1,1}; stack_push("{} + {}", 2, arg_reps);}
   | expr '-' expr { rpn("SUB"); int arg_reps[] = {1,1}; stack_push("{} - {}", 2, arg_reps);}
   | expr '*' expr { rpn("MUL"); int arg_reps[] = {1,1}; stack_push("{} * {}", 2, arg_reps);}
   | expr '/' expr { rpn("DIV"); int arg_reps[] = {1,1}; stack_push("{} / {}", 2, arg_reps);}
   | expr '%' expr { rpn("MOD"); int arg_reps[] = {1,1}; stack_push("{} % {}", 2, arg_reps);}
   | expr MOD expr { rpn("MOD"); int arg_reps[] = {1,1}; stack_push("{} MOD {}", 2, arg_reps);}
   | expr ANDOP expr { rpn("AND"); int arg_reps[] = {1,1}; stack_push("{} AND {}", 2, arg_reps);}
   | expr OR expr { rpn("OR"); int arg_reps[] = {1,1}; stack_push("{} OR {}", 2, arg_reps);}
   | expr XOR expr { rpn("XOR"); int arg_reps[] = {1,1}; stack_push("{} XOR {}", 2, arg_reps);}
   | expr '|' expr { rpn("BITOR"); int arg_reps[] = {1,1}; stack_push("{} | {}", 2, arg_reps);}
   | expr '&' expr { rpn("BITAND"); int arg_reps[] = {1,1}; stack_push("{} & {}", 2, arg_reps);}
   | expr '^' expr { rpn("BITXOR"); int arg_reps[] = {1,1}; stack_push("{} ^ {}", 2, arg_reps);}
   | NOT expr { $$ = $2 + 1; rpn("NOT"); int arg_reps[] = {1}; stack_push("NOT {}", 1, arg_reps);}
   | '!' expr { $$ = $2 + 1; rpn("NOT"); int arg_reps[] = {1}; stack_push("! {}", 1, arg_reps);}
   | expr COMPARISON expr {
    rpn("CMP %s", $2);
    int arg_reps[] = {1,1}; stack_pushf("{} %s {}", 2, arg_reps, 16, $2);
    free($2);
 }

      /* recursive selects and comparisons */
   | expr COMPARISON '(' select_stmt ')' {
    rpn("CMPSELECT %d", $2);
    int arg_reps[] = {}; stack_pushf("{} %s ({})", 2, arg_reps, 16, $2);
    free($2);
 }
   | expr COMPARISON ANY '(' select_stmt ')' {
    rpn("CMPANYSELECT %d", $2);
    int arg_reps[] = {}; stack_pushf("{} %s ANY ({})", 2, arg_reps, 16, $2);
    free($2);
 }
   | expr COMPARISON SOME '(' select_stmt ')' {
    rpn("CMPANYSELECT %d", $2);
    int arg_reps[] = {}; stack_pushf("{} %s SOME ({})", 2, arg_reps, 16, $2);
    free($2);
 }
   | expr COMPARISON ALL '(' select_stmt ')' {
    rpn("CMPALLSELECT %d", $2);
    int arg_reps[] = {}; stack_pushf("{} %s ALL ({})", 2, arg_reps, 16, $2);
    free($2);
 }
   ;

expr:  expr IS NULLX     { rpn("ISNULL"); int arg_reps[] = {}; stack_push("{} IS NULL", 1, arg_reps);}
   | expr IS NOT NULLX { rpn("ISNULL"); rpn("NOT"); int arg_reps[] = {}; stack_push("{} IS NOT NULL", 1, arg_reps);}
   | expr IS BOOL      { rpn("ISBOOL %d", $3); int arg_reps[] = {}; stack_push("{} IS BOOL", 1, arg_reps);}
   | expr IS NOT BOOL  { rpn("ISBOOL %d", $4); rpn("NOT"); int arg_reps[] = {}; stack_push("{} IS NOT BOOL", 1, arg_reps);}

   | USERVAR ASSIGN expr { rpn("ASSIGN @%s", $1);  int arg_reps[] = {}; stack_push("ASSIGN @{}", 1, arg_reps); free($1);}
   ;

    /* "AND" has special precedence in BETWEEN clauses */
expr: expr BETWEEN expr AND expr %prec BETWEEN { rpn("BETWEEN"); int arg_reps[] = {1,1,1}; stack_push("{} BETWEEN {} AND {}", 3, arg_reps);}
   ;

// SELECT

select_stmt: SELECT select_opts select_expr_list
                        { $$ = 1; rpn("SELECTNODATA %d %d", $2, $3); int arg_reps[] = {$2, $3}; stack_push("SELECT {}{}", 2, arg_reps); }
;
select_stmt: SELECT select_opts select_expr_list
                        { $$ = 1; rpn("SELECTNODATA %d %d", $2, $3); int arg_reps[] = {$2, $3}; stack_push("SELECT {}{}", 2, arg_reps); }
    | SELECT select_opts select_expr_list
     FROM table_references
     opt_where opt_groupby opt_having opt_orderby opt_limit
     opt_into_list { rpn("SELECT %d %d %d", $2, $3, $5); } ;
;

select_opts:                          { $$ = 0; int arg_reps[] = {}; stack_push("", 0, arg_reps);}
| select_opts ALL                 
   { rpn("ALL"); int arg_reps[] = {}; stack_push("{}ALL ", 1, arg_reps); }
| select_opts DISTINCT            
   { rpn("DISTINCT"); int arg_reps[] = {}; stack_push("{}DISTINCT ", 1, arg_reps); }
| select_opts DISTINCTROW         
   { rpn("DISTINCTROW"); int arg_reps[] = {}; stack_push("{}DISTINCTROW ", 1, arg_reps); }
| select_opts STRAIGHT_JOIN       
   { rpn("STRAIGHT_JOIN"); int arg_reps[] = {}; stack_push("{}STRAIGHT_JOIN ", 1, arg_reps); }
    ;

select_expr_list: select_expr { $$ = 1; int arg_reps[] = {1}; stack_push("{}", 1, arg_reps);}
    | select_expr_list ',' select_expr {$$ = $1 + 1; int arg_reps[] = {$1, 1}; stack_push("{}, {}", 2, arg_reps);}
    | '*' { rpn("SELECTALL"); $$ = 1; int arg_reps[] = {}; stack_push("*", 0, arg_reps); }
    ;

select_expr: expr opt_as_alias {int arg_reps[] = {1, 1}; stack_push("{}{}", 2, arg_reps); } ;

opt_as_alias: AS NAME { rpn ("ALIAS %s", $2); int arg_reps[] = {1}; stack_push("AS {}", 1, arg_reps); free($2); }
  | NAME              { rpn ("ALIAS %s", $1); int arg_reps[] = {}; stack_push("{}", 1, arg_reps); free($1); }
  | /* nil */ { int arg_reps[] = {}; stack_push("", 0, arg_reps); }
  ;

// FROM
table_references:    table_reference { $$ = 1; }
    | table_references ',' table_reference { $$ = $1 + 1; }
    ;

table_reference:  table_factor
  | join_table
;

table_factor:
    NAME opt_as_alias { rpn("TABLE %s", $1); free($1); }
  | NAME '.' NAME opt_as_alias { rpn("TABLE %s.%s", $1, $3);
                               free($1); free($3); }
  | table_subquery opt_as NAME { rpn("SUBQUERYAS %s", $3); free($3); }
  | '(' table_references ')' { rpn("TABLEREFERENCES %d", $2); }
  ;

opt_as: AS
  | /* nil */
  ;


// WHERE
opt_where: /* nil */
   | WHERE expr { rpn("WHERE"); };

// GROUP BY
opt_groupby: /* nil */
   | GROUP BY groupby_list opt_with_rollup
                             { rpn("GROUPBYLIST %d %d", $3, $4); }
;

groupby_list: expr opt_asc_desc
                             { rpn("GROUPBY %d",  $2); $$ = 1; }
   | groupby_list ',' expr opt_asc_desc
                             { rpn("GROUPBY %d",  $4); $$ = $1 + 1; }
   ;

opt_asc_desc: /* nil */ { $$ = 0; }
   | ASC                { $$ = 0; }
   | DESC               { $$ = 1; }
    ;

opt_with_rollup: /* nil */  { $$ = 0; }
   | WITH ROLLUP  { $$ = 1; }
   ;

// HAVING
opt_having: /* nil */
   | HAVING expr { rpn("HAVING"); };

opt_orderby: /* nil */
   | ORDER BY groupby_list { rpn("ORDERBY %d", $3); }
   ;

opt_limit: /* nil */ | LIMIT expr { rpn("LIMIT 1"); }
  | LIMIT expr ',' expr             { rpn("LIMIT 2"); }
  ;

// INTO
opt_into_list: /* nil */
   | INTO column_list { rpn("INTO %d", $2); }
   ;

column_list: NAME { rpn("COLUMN %s", $1); free($1); $$ = 1; }
  | column_list ',' NAME  { rpn("COLUMN %s", $3); free($3); $$ = $1 + 1; }
  ;

// JOIN
join_table:
    table_reference opt_inner_cross JOIN table_factor opt_join_condition
                  { rpn("JOIN %d", 100+$2); }
  | table_reference STRAIGHT_JOIN table_factor
                  { rpn("JOIN %d", 200); }
  | table_reference STRAIGHT_JOIN table_factor ON expr
                  { rpn("JOIN %d", 200); }
  | table_reference left_or_right opt_outer JOIN table_factor join_condition
                  { rpn("JOIN %d", 300+$2+$3); }
  | table_reference NATURAL opt_left_or_right_outer JOIN table_factor
                  { rpn("JOIN %d", 400+$3); }
  ;

opt_inner_cross: /* nil */ { $$ = 0; }
   | INNER { $$ = 1; }
   | CROSS  { $$ = 2; }
;

opt_outer: /* nil */  { $$ = 0; }
   | OUTER {$$ = 4; }
   ;

left_or_right: LEFT { $$ = 1; }
    | RIGHT { $$ = 2; }
    ;

opt_left_or_right_outer: LEFT opt_outer { $$ = 1 + $2; }
   | RIGHT opt_outer  { $$ = 2 + $2; }
   | /* nil */ { $$ = 0; }
   ;

opt_join_condition: /* nil */
   | join_condition ;

join_condition:
    ON expr { rpn("ONEXPR"); }
    | USING '(' column_list ')' { rpn("USING %d", $3); }
    ;

// SUBQUERIES
table_subquery: '(' select_stmt ')' { rpn("SUBQUERY"); }
   ;

%%


char* itostr(int i) {
  char* str = malloc(sizeof(char) * 64);
  sprintf(str, "%d", i);
  return str;
}

char* btostr(int b) {
  switch(b) {
    case 1:
      return "TRUE";
    case 0:
      return "FALSE";
    default:
      return "UNKNOWN";
  }
}

char* ftostr(float f) {
  char* str = malloc(sizeof(char) * 64);
  sprintf(str, "%f", f);
  return str;
}


void rpn(char* s, ...) {
  va_list ap;
  va_start(ap, s);

  if ( print_rpn ) {
    vfprintf(stdout, "rpn: ", ap);
    vfprintf(stdout, s, ap);
    vfprintf(stdout, "\n", ap);
  }
}


void stack_push(char* s, int numargs, int arg_reps[]) {
  char* str = malloc(sizeof(char) * BUFF_SIZE);
  struct stack_entry* entry = malloc(sizeof* entry);
  memcpy(entry->arg_reps, arg_reps, numargs * sizeof(int));
  strcpy(str, s);
  entry->fstring = str;
  entry->numargs = numargs;

  SLIST_INSERT_HEAD(&stack_head, entry, entries);
}


void stack_pushf(char* s, int numargs, int arg_reps[], int maxsize, ...) {
  va_list ap;
  va_start(ap, maxsize);

  char* str = malloc(sizeof(char) * maxsize);
  vsnprintf(str, maxsize, s, ap);
  stack_push(str, numargs, arg_reps);
}

void debug_print(int depth, char * tmpl, ...) {
  va_list ap;
  va_start(ap, tmpl);

  int n = 2;
  if (yydebug) {
    char* pref = malloc(sizeof(char) * (BUFF_SIZE * n));
    int i = 0;
    while (i < depth) {
      for (int j = 0; j < n; j++)
        pref[i*n+j] = '|';

      i++;
    }
    pref[i*n] = '\0';

    char* str = malloc(depth * n + 1 + BUFF_SIZE);
    *str = '\0';
    strcat(str, pref);
    strcat(str, tmpl);

    vprintf(str, ap);

    free(pref);
    free(str);

  }
}


char* construct(char* buff, int depth) {
  /*
    Pops the head off the RPN stack and recursively constructs a string for each argument of the current RPN operator.
    The number of arguments to construct is given by entry->numargs. Since the stack is in RPN order, the strings for the arguments are constructed in reverse order.
    After strings for all the arguments are constructed, the whole string is written to buff.
  */
    
  // Pop the stack
  struct stack_entry* entry = SLIST_FIRST(&stack_head);
  SLIST_REMOVE_HEAD(&stack_head, entries);

  debug_print(depth, "CONSTRUCTING ENTRY: '%s'\n", entry->fstring);

  // Calculate the total no. of arguments
  debug_print(depth, "NUMARGS: %i\n", entry->numargs);
  int argsum = 0;
  for (int i = 0; i < entry->numargs; i++) {
    debug_print(depth, "NUMARG: %i\n", entry->arg_reps[i]);
    argsum += entry->arg_reps[i];
  }
  debug_print(depth, "ARGSUM: %i\n", argsum);

  // Allocate a buffer for each argument
  char **arg_buffs;
  arg_buffs = malloc(argsum * sizeof(char*));
  for (int i = 0; i < argsum; i++)
    arg_buffs[i] = malloc(sizeof(char) * BUFF_SIZE);

  // From the end (because RPN), construct and store into buffers.
  for (int i = argsum - 1; i > -1; i--) {
    strcpy(arg_buffs[i], "");
    debug_print(depth, "ENTERING ARG[%i]\n", i);
    construct(arg_buffs[i], depth + 1);
    debug_print(depth, "ARG[%i] CONSTRUCTED: '%s'\n", i, arg_buffs[i]);
  }
  debug_print(depth, "RETURNED TO STACK ENTRY '%s'\n", entry->fstring);

  char *found_char, *tmpl_str = entry->fstring;
  int found_ind, str_start = 0;

  // This could've gone in with the loop above to minimize recursion - but no time to optimize
  int curr_arg_num = 0;
  for (int i = 0; i < entry->numargs; i++) {
    // we look for each occurence of "{}" in the template string and substitute it for the constructed argument strings for the argument cluster
    found_char = strchr(&tmpl_str[str_start], '{');
    found_ind = (int)(found_char - tmpl_str);
    if (tmpl_str[found_ind + 1] == '}') {

      // First, insert the substring before the argument
      strncat(buff, &tmpl_str[str_start], found_ind - str_start);

      // Append all the arguments
      for (int j = 0; j < entry->arg_reps[i]; j++) {
        debug_print(depth, "APPENDING ARG[%d][%d] CURR_ARG_NUM: %i\nARG VAL: '%s'\n", i, j, curr_arg_num, arg_buffs[curr_arg_num]);
        strcat(buff, arg_buffs[curr_arg_num++]);
      }

      str_start = found_ind + 2;
    } else {
      yyerror("Could not find closing parenthesis in the template string.");
    }
  }
  // Append what is left of the template string
  strncat(buff, &tmpl_str[str_start], (int)strlen(tmpl_str) - str_start);

  debug_print(depth, "ENTRY '%s' CONSTRUCTED, RES = '%s'\n", entry->fstring, buff);

  free(entry);
  for (int i = 0; i < argsum; i++)
    free(arg_buffs[i]);
  free(arg_buffs);
}


void pprint() {
  char* buff = malloc(sizeof(char) * BUFF_SIZE);
  strcpy(buff, "");
  construct(buff, 0);
  printf("%s", buff);
  free(buff);
}


int yyerror(const char *s, ...) {
  int yylineno;

  va_list ap;
  va_start(ap, s);

  fprintf(stderr, "%d: error: ", yylineno);
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}


int main(int ac, char **av) {
  SLIST_INIT(&stack_head);

  extern FILE *yyin;

  if(ac > 1 && !strcmp(av[1], "-d")) {
    ac--; av++; // yydebug = 1;
  }

  if(ac > 1 && (yyin = fopen(av[1], "r")) == NULL) {
    perror(av[1]);
    exit(1);
  }

  if(!yyparse()) {
    pprint();
    if (yydebug) printf("\nSQL parse worked\n");
  }
  else {
    if (yydebug) printf("\nSQL parse failed\n");
    return 1;
  }

  return 0;
}
