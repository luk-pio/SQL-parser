%{
    #include <stdlib.h>
    #include <stdarg.h>
    #include <stddef.h>
    #include <stdio.h>
    #include <string.h>
    #include <math.h>
    #include <sys/queue.h>

    int yydebug;

    int yylex(void);
    void rpn(char *s, ...);
    void stack_push(char *s, int numargs);
    char *itostr(int num);
    int yyerror(const char *s, ...);

    /* SLIST_HEAD(listhead, rpn_entry) head; */

    /* struct rpn_entry { */
    /*   char token[100]; */
    /*   char attr[100]; */
    /*   int num_args; */
    /*   SLIST_ENTRY(rpn_entry) entries; */
    /* }; */


    struct stack_entry {
      char *fstring;
      int numargs;
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

// Keywords

%token SELECT
%token FROM

// names and literals

%token <strval> NAME
%token <strval> STRING
%token <intval> INTNUM
%token <intval> BOOL
%token <floatval> APPROXNUM

%token <strval> USERVAR

%type <intval> expr
%type <intval> select_expr

%type <strval> stmt_list
%type <strval> stmt

%%

// TOP
top: stmt_list;

stmt_list: stmt ';' 
| stmt_list stmt ';' { stack_push("{}{}", 2); }
;

stmt: select_stmt { rpn("STMT"); stack_push("{};\n", 1); };

expr: NAME         { rpn("NAME %s", $1); stack_push($1, 0); }
   | NAME '.' NAME { rpn("FIELDNAME %s.%s", $1, $3); free($1); free($3); }
   | USERVAR       { rpn("USERVAR %s", $1); free($1); }
   | STRING        { rpn("STRING %s", $1); free($1); }
   | INTNUM        { rpn("INTNUM %i", $1); stack_push(itostr($1), 0); }
   | APPROXNUM     { rpn("FLOAT %g", $1); }
   | BOOL          { rpn("BOOL %d", $1); }
   ;

// SELECT

select_stmt: SELECT select_expr
                        { rpn("SELECT, %d", $2); stack_push("SELECT {}", 1); }
;

/*     | SELECT select_opts select_expr_list  */
/*      FROM table_references */
/*      opt_where opt_groupby opt_having opt_orderby opt_limit */
/*      opt_into_list { rpn("SELECT %d %d %d", $2, $3, $5); } ; */
/* ;    */

select_expr: expr;

//
// JOIN
//
// WHERE
//
// ORDER By
//
// GROUP BY
//
// NESTED SUBQUERIES

%%

char *itostr(int num) {
  static char str[50];
  sprintf(str, "%d", num);
  return str;
}

void rpn(char *s, ...) {
  va_list ap;
  va_start(ap, s);

  if ( yydebug ) {
    vfprintf(stdout, "rpn: ", ap);
    vfprintf(stdout, s, ap);
    vfprintf(stdout, "\n", ap);
  }
}

void stack_push(char* s, int numargs) {
  struct stack_entry* entry = malloc(sizeof* entry);
  entry->fstring = s;
  entry->numargs = numargs;

  SLIST_INSERT_HEAD(&stack_head, entry, entries);
}

char* construct(char* buff) {
  // Pop the stack
  struct stack_entry* entry = SLIST_FIRST(&stack_head);
  SLIST_REMOVE_HEAD(&stack_head, entries);

  if (yydebug) {
    printf("ENTRY: %s\n", entry->fstring);
    printf("BUFF: %s\n", buff);
  }

  char* tmpl_str = entry->fstring;
  char* found_char;
  char* tmp_buff;
  int found_ind;
  int start = 0;
  char* arg_buffs[entry->numargs];


  // From the end, construct and store into buffers.
  for (int i = entry->numargs - 1; i > -1; i--) {
    arg_buffs[i] = construct(buff);
    start = found_ind - 2*sizeof(char);
  }


  // This could've gone in with the loop above but seems simpler and more readable this way
  for (int i = 0; i < entry->argnums; i++) {
    found_char = strchr(&tmpl_str[start], '{');
    found_ind = (int)(found_char + tmpl_str);
    if (tmpl_str[found_ind + sizeof(char)] == '}') {

      if (yydebug) printf("Constructing arg %i: %i - %i\n", entry->numargs - i, start, found_ind - start);

      strncat(buff, &tmpl_str[start], found_ind - start);

    }
    strncat(buff, &entry->fstring[start], strlen(tmpl_str) - start);

  }

  free(entry);
}

void pprint() {
  char buff[1024], cp_buff[1024], tmp_buff[1024];
  struct stack_entry *entry;
  construct(buff);
  printf("OUTPUT: %s", buff);
    
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
    yydebug = 1; ac--; av++;
  }

  if(ac > 1 && (yyin = fopen(av[1], "r")) == NULL) {
    perror(av[1]);
    exit(1);
  }

  
  if(!yyparse()) {
    pprint();
    printf("\nSQL parse worked\n");
  }
  else {
    printf("\nSQL parse failed\n");
    return 1;
  }

  return 0;
}
