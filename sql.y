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
    void pprint_push(char *s, ...);
    char *itostr(int num);
    int yyerror(const char *s, ...);

    /* SLIST_HEAD(listhead, rpn_entry) head; */

    /* struct rpn_entry { */
    /*   char token[100]; */
    /*   char attr[100]; */
    /*   int num_args; */
    /*   SLIST_ENTRY(rpn_entry) entries; */
    /* }; */


    struct pp_entry {
      char *fstring;
      SLIST_ENTRY(pp_entry) entries;
    };

    SLIST_HEAD(listhead, pp_entry);
    struct listhead pp_head;


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

%start stmt_list

%%

// TOP
stmt_list: stmt ';' 
| stmt_list stmt ';' {pprint_push("%s%s", $1, $2);}
;

stmt: select_stmt { rpn("STMT"); pprint_push("%s;\n"); };

expr: NAME         { rpn("NAME %s", $1); pprint_push($1); }
   | NAME '.' NAME { rpn("FIELDNAME %s.%s", $1, $3); free($1); free($3); }
   | USERVAR       { rpn("USERVAR %s", $1); free($1); }
   | STRING        { rpn("STRING %s", $1); free($1); }
   | INTNUM        { rpn("INTNUM %i", $1); pprint_push(itostr($1)); }
   | APPROXNUM     { rpn("FLOAT %g", $1); }
   | BOOL          { rpn("BOOL %d", $1); }
   ;

// SELECT

select_stmt: SELECT select_expr
                        { rpn("SELECT, %d", $2); pprint_push("SELECT %s"); }
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

void pprint_push(char *s, ...) {
  struct pp_entry *entry;
  entry = malloc(sizeof(struct pp_entry));      /* Insert at the head. */
  entry->fstring = s;
  SLIST_INSERT_HEAD(&pp_head, entry, entries);
}

void pprint() {
  char buff[1024], tmp_buff[1024];

  // Take the first element off the stack and use it as the root template string.
  struct pp_entry *entry;
  entry = SLIST_FIRST(&pp_head);
  sprintf(buff, "%s", entry->fstring);
  printf("ENTRY: %s\n", entry->fstring);
  printf("BUFF: %s\n", buff);
  SLIST_REMOVE_HEAD(&pp_head, entries);
  free(entry);

  SLIST_FOREACH(entry, &pp_head, entries) {
    printf("ENTRY: %s\n", entry->fstring);
    printf("BUFF: %s\n", buff);
    // Use a temp buffer to prevent losing \0
    strcpy(tmp_buff, buff);
    sprintf(buff, tmp_buff, entry->fstring);
  }

  printf("%s", buff);
    
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
  SLIST_INIT(&pp_head);

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
/* main(int argc, char *argv[]) { */
/*     yyin = stdin; */
/*     yyout = stdout; */

/*     /\* Jezeli zostal zdefiniowany plik wejscia i/lub wyjscia *\/ */
/*     if (argc >= 2) */
/*     { */
/*         /\* Otworz plik do czytania *\/ */
/*         yyin = fopen(argv[1], "r"); */

/*         /\* Jezeli nie udalo sie otworzyc pliku *\/ */
/*         if(yyin == NULL) */
/*         { */
/*             fprintf(stderr, "ERROR: Input file not exists.\n"); */
/*             return 1; */
/*         } */
/*     } */

/*     yyparse(); */
/*     if(test){ */
/*         printf("Test OK!\n"); */
/*         fprintf(yyout, "Test OK!\n"); */
/*     } */
/* } */
