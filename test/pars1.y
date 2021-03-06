%{     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 30 Jul 13   */
/* Copyright (c) 2013 Gordon S. Novak Jr. and
   The University of Texas at Austin. */
/* 14 Feb 01; 01 Oct 04; 02 Mar 07; 27 Feb 08; 24 Jul 09; 02 Aug 12 */
/*
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
  */
/* NOTE:   Copy your lexan.l lexical analyzer to this directory.      */
       /* To use:
                     make pars1y              has 1 shift/reduce conflict
                     pars1y                   execute the parser
                     i:=j .
                     ^D                       control-D to end input
                     pars1y                   execute the parser
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.
                     ^D
                     pars1y                   execute the parser
                     if x+y then if y+z then i:=j else k:=2.
                     ^D
           You may copy pars1.y to be parse.y and extend it for your
           assignment.  Then use   make parser   as above.
        */
        /* Yacc reports 1 shift/reduce conflict, due to the ELSE part of
           the IF statement, but Yacc's default resolves it in the right way.*/
#include <stdio.h>
#include <ctype.h>
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "parse.h"
        /* define the type of the Yacc stack element to be TOKEN */
#define YYSTYPE TOKEN
TOKEN parseresult;
%}

/* Order of tokens corresponds to tokendefs.c; do not change */

%token IDENTIFIER STRING NUMBER   /* token types */

%token PLUS MINUS TIMES DIVIDE    /* Operators */
%token ASSIGN EQ NE LT LE GE GT POINT DOT AND OR NOT DIV MOD IN

%token COMMA                      /* Delimiters */
%token SEMICOLON COLON LPAREN RPAREN LBRACKET RBRACKET DOTDOT

%token ARRAY BEGINBEGIN           /* Lex uses BEGIN */
%token CASE CONST DO DOWNTO ELSE END FILEFILE FOR FUNCTION GOTO IF LABEL NIL
%token OF PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL
%token VAR WHILE WITH


%%

  program    : PROGRAM type LPAREN type RPAREN SEMICOLON vars statement DOT { parseresult = makeprogram($2, $4, $8); }
             ;
  layout     : constants layout    
             | constants             
             ;
  vars       : VAR varLayout                      
             | CONST layout vars                 
             ;          
  varLayout  : varname COLON type SEMICOLON    { instvars($1, $3); }               
             | varname COLON type SEMICOLON varLayout       { instvars($1, $3); }
             ;
  varname    : type COMMA varname              { $$ = cons($1, $3); }
             | type                   
             ;
  constants  : type EQ NUMBER SEMICOLON        { instconst($1, $3);}
             ;
  type       : IDENTIFIER                      { $$ = findtype($1);}
             ;
 //get loop and calls------------------------------------------------------             
  loop       :  FOR assignment TO type DO statement { $$ = makefor(1, talloc(), $2, findid($4), NULL, NULL, $6); }
             |  IF expr THEN statement endif   { $$ = makeif($1, $2, $4, $5); }                        
             |  call         
             |  REPEAT statement UNTIL equals  { $$ = makerepeat(talloc(), $2, (TOKEN) talloc(), $4); }                        
             |  assignment                    
             ;
  equals     : type EQ NUMBER           { $$ = binop($2, $1, $3);  }
             ;
  call       : type LPAREN expr RPAREN  { $$ = makefuncall(talloc(), findid($1), $3); }
             ; 
 //given rules-----------------------------------------------------------------      
  statement  :  BEGINBEGIN statement endpart   {  $$ = makeprogn($1, $2);  }
             |  loop SEMICOLON statement       {  $$ = cons($1, $3); }
             |  loop                           
             ;      
  endpart    :  SEMICOLON statement endpart    { $$ = cons($2, $3); }
             |  END                            { $$ = NULL; }
             ;
  endif      :  ELSE statement                 { $$ = $2; }
             ;
  assignment :  IDENTIFIER ASSIGN expr         { $$ = binop($2, $1, $3); }
             ;
  expr       :  MINUS term                     { $$ = unaryop($1, $2); }
             |  expr MINUS term                { $$ = binop($2, $1, $3); }
             |  expr PLUS term                 { $$ = binop($2, $1, $3); }
             |  term                           { $$ = $1; }
             ;
  term       :  term TIMES factor              { $$ = binop($2, $1, $3); }
             |  factor                         { $$ = $1; }
             ;
  factor     :  LPAREN expr RPAREN             { $$ = $2; }
             |  call                           { $$ = $1; }   
             |  IDENTIFIER                     { $$ = findid($1); }
             |  NUMBER                         { $$ = $1; }                          
             |  STRING                         { $$ = $1; }        
             ;

%%

/* You should add your own debugging flags below, and add debugging
   printouts to your programs.

   You will want to change DEBUG to turn off printouts once things
   are working.
  */

#define DEBUG         0             /* set bits here for debugging, 0 = off  */
#define DB_CONS       1             /* bit to trace cons */
#define DB_BINOP      2             /* bit to trace binop */
#define DB_MAKEIF     4             /* bit to trace makeif */
#define DB_MAKEPROGN  8             /* bit to trace makeprogn */
#define DB_PARSERES  16             /* bit to trace parseresult */
#define DB_MAKEFOR    1             /* bit to trace makefor */

 int labelnumber = 0;  /* sequential counter for internal label numbers */

   /*  Note: you should add to the above values and insert debugging
       printouts in your routines similar to those that are shown here.     */

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { item->link = list;
    if (DEBUG & DB_CONS)
       { printf("cons\n");
         dbugprinttok(item);
         dbugprinttok(list);
       };
    return item;
  }

/* unaryop links a unary operator op to one operand, lhs */
TOKEN unaryop(TOKEN op, TOKEN lhs) {
    op->operands = lhs;          /* link operands to operator       */
    lhs->link = NULL;
    return op;
}

/*checks datatype of tok symbol*/
int checkDT(TOKEN tok, int checkINT) {
  SYMBOL sym = searchst(tok->stringval);
  if (checkINT && sym->basicdt == INTEGER) 
    return 1;
  else if (!checkINT && sym->basicdt == REAL) 
    return 1;
  return 0;
}

/* binop links a binary operator op to two operands, lhs and rhs. */
TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs) {        /* reduce binary operator */ 
  op->operands = lhs;          /* link operands to operator       */
  lhs->link = rhs;             /* link second operand to first    */
  rhs->link = NULL;            /* terminate operand list          */
  
  int rhs_id = 0;
  if(rhs->tokentype == IDENTIFIERTOK)
    rhs_id = 1;

  if (rhs_id && (lhs->datatype == REAL)) { 
    if (checkDT(rhs, 0) || checkDT(rhs, 1)) 
      op->datatype = REAL;
    if (checkDT(rhs, 1)) {
      TOKEN temp = makeop(FLOATOP);
      temp->operands = rhs;
      lhs->link = temp;
    } 
  }

  if (rhs_id && (lhs->datatype == INTEGER) && checkDT(rhs, 0)) {
    double d = lhs-> intval;
    op->datatype = REAL;
    lhs->datatype = REAL;
    lhs->realval = d;
  } 

  if (DEBUG & DB_BINOP)
     { printf("binop\n");
       dbugprinttok(op);
       dbugprinttok(lhs);
       dbugprinttok(rhs);
     };
  return op;
  }

/* makerepeat makes structures for a repeat statement.
   tok and tokb are (now) unused tokens that are recycled. */
TOKEN makerepeat(TOKEN tok, TOKEN statements, TOKEN tokb, TOKEN expr) {
  int labelnum = labelnumber;
  TOKEN tempLabel = makelabel();
  TOKEN tokProg = makeprogn(tokb, statements);
  TOKEN tokGOTO = makegoto(labelnum);
  TOKEN tokEmpty = makeprogn((TOKEN) talloc(), NULL);
  TOKEN tokIF = talloc();

  tok = makeprogn(tok, tempLabel);
  tokIF = makeif(tokIF, expr, tokEmpty, tokGOTO);

  tempLabel->link = tokProg;
  tokEmpty->link = tokGOTO;
  tokProg->link = tokIF;

  return tok;  
}

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart)
  {  tok->tokentype = OPERATOR;  /* Make it look like an operator   */
     tok->whichval = IFOP;
     if (elsepart != NULL) elsepart->link = NULL;
     thenpart->link = elsepart;
     exp->link = thenpart;
     tok->operands = exp;
     return tok;
   }

/* makeprogn makes a PROGN operator and links it to the list of statements.
   tok is a (now) unused token that is recycled. */
TOKEN makeprogn(TOKEN tok, TOKEN statements)
  {  tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
     return tok;
   }

/* findid finds an identifier in the symbol table and sets up
the symbol table pointers */
/* taken from lecture notes pg 127 */
TOKEN findid(TOKEN tok) { /* the ID token */
   SYMBOL sym, typ;
   sym = searchst(tok->stringval);
   tok->symentry = sym;
   typ = sym->datatype;
   tok->symtype = typ;

    if ( typ->kind == BASICTYPE || typ->kind == POINTERSYM)
      tok->datatype = typ->basicdt; 
    if (sym->kind == CONSTSYM && sym->basicdt == INTEGER) {
      tok->tokentype = NUMBERTOK;
      tok->datatype = INTEGER;
      tok->intval = sym->constval.intnum;
    }
    if (sym->kind == CONSTSYM && sym->basicdt == REAL) {
      tok->tokentype = NUMBERTOK;
      tok->datatype = REAL;
      tok->realval = sym->constval.realnum;
    }
   return tok;
}

/* findtype looks up a type name in the symbol table, puts the pointer
   to its type into tok->symtype, returns tok. */
TOKEN findtype(TOKEN tok) {
  tok->symtype = searchst(tok->stringval);
  return tok;
}

/* instconst installs a constant in the symbol table */
void instconst(TOKEN idtok, TOKEN consttok) {
    SYMBOL sym, typesym;
    int align = alignsize(typesym);
    
    sym = insertsym(idtok->stringval);
    sym->kind = CONSTSYM;
    sym->size = typesym->size;
    sym->datatype = typesym;
    sym->basicdt = consttok->datatype;
    if(sym->basicdt == REAL) //real
        sym->constval.realnum = consttok->realval;
    if(sym->basicdt == INTEGER) //int
        sym->constval.intnum = consttok->intval;
}

/* install variables in symbol table */
/* taken from lecture notes pg 133 */
void instvars(TOKEN idlist, TOKEN typetok) {
  //blockoffs[blocknumber] is the offset in the current block
  // this is the next value for this storage allocation
  SYMBOL sym, typesym; int align;
  typesym = typetok->symtype;
  align = alignsize(typesym);
  while ( idlist != NULL )   /* for each id */
    {  sym = insertsym(idlist->stringval);
       sym->kind = VARSYM;
       sym->offset = wordaddress(blockoffs[blocknumber], align);
       sym->size = typesym->size;

       blockoffs[blocknumber] = sym->offset + sym->size;
       sym->datatype = typesym;
       sym->basicdt = typesym->basicdt;
       idlist = idlist->link;
    };
}

int wordaddress(int n, int wordsize)
  { return ((n + wordsize - 1) / wordsize) * wordsize; }
 
yyerror(s)
  char * s;
  { 
  fputs(s,stderr); putc('\n',stderr);
  }

/* makefuncall makes a FUNCALL operator and links it to the fn and args.
   tok is a (now) unused token that is recycled. */
TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args){
  tok->operands = fn;
  fn->link = args;

  //looking at tok vals need to make changes
  tok->datatype = fn->datatype;
  tok->whichval = FUNCALLOP;

  return tok;
}  

/* makeprogram makes the tree structures for the top-level program */
TOKEN makeprogram(TOKEN name, TOKEN args, TOKEN statements) {
  TOKEN maketree = talloc();
  maketree = makeprogn(maketree, args);
  name->link = maketree;

  TOKEN treest = talloc();
  treest->tokentype = OPERATOR;
  treest->operands = name;
  treest->whichval = PROGRAMOP;
  maketree->link = statements;

  return treest;
}

/* copytok makes a new token that is a copy of origtok */
TOKEN copytok(TOKEN origtok) {
  //need to set all of newtok attributes to origtok
  TOKEN newtok = talloc();

  newtok->datatype = origtok->datatype;
  newtok->symtype = origtok->symtype;
  newtok->link = origtok->link;
  newtok->tokentype = origtok->tokentype;
  newtok->whichval = origtok->whichval;

  newtok->intval = origtok->intval;
  newtok->symentry = origtok->symentry;
  newtok->realval = origtok->realval;
  return newtok;
}

/* makelabel makes a new label, using labelnumber++ */
TOKEN makelabel() {
  //probably need makeintc.. increase labelnumber
  TOKEN labltok = talloc();
  labltok->tokentype = OPERATOR;
  labltok->operands = makeintc(labelnumber);
  labltok->whichval = LABELOP;
  
  //gets a new label for next time
  labelnumber++;
  return labltok;
}  

/* makeop makes a new operator token with operator number opnum.
   Example:  makeop(FLOATOP)  */
TOKEN makeop(int opnum){
  //need to check if i need to set anything for tok
  TOKEN tok = talloc();
    tok->whichval = opnum;
  tok->tokentype = OPERATOR;

  return tok;
}  

/* makegoto makes a GOTO operator to go to the specified label.
   The label number is put into a number token. */
TOKEN makegoto(int label) {
  int val = labelnumber - 1;

  TOKEN tok_goto = talloc();
  tok_goto->tokentype = OPERATOR;

  tok_goto->operands = makeintc(val);
  tok_goto->whichval = GOTOOP;
  return tok_goto;
}

/* makeintc makes a new token with num as its value */
TOKEN makeintc(int num) {
  TOKEN newint = talloc();
  newint->tokentype = NUMBERTOK;
  newint->intval = num;
  newint->datatype = INTEGER;
  
  return newint;
}

/* makefor makes structures for a for statement.
   sign is 1 for normal loop, -1 for downto.
   asg is an assignment statement, e.g. (:= i 1)
   endexpr is the end expression
   tok, tokb and tokc are (now) unused tokens that are recycled. */
TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr,
              TOKEN tokc, TOKEN statement) {
  //sign will always be 1 for trivb.pas b/c using a normal for loop
  //need to change implementation for future projects  

  TOKEN newLabel = makelabel();
  TOKEN iftok =  talloc();
  TOKEN progntok = talloc();
  TOKEN onetok = makeintc(1);
  TOKEN ASSIGNtok = makeop(ASSIGNOP);
  TOKEN LEtok = makeop(LEOP);
  TOKEN PLUStok = makeop(PLUSOP);
  TOKEN temp1 = copytok(asg->operands);
  TOKEN temp2 = copytok(temp1);
  TOKEN temp3 = copytok(temp1);

  statement->link = ASSIGNtok;
  ASSIGNtok->link = makegoto(labelnumber - 1);

  tok = makeprogn(tok, asg);
  tok->operands = asg;
  //creates the new label
  asg->link = newLabel;

  progntok = makeprogn(progntok, statement);
  iftok = makeif(iftok, LEtok, progntok, NULL);
  iftok->operands = LEtok;
  newLabel->link = iftok;

  //need to establish correct links with temp tokens for tree
  LEtok->operands = temp1;
  temp1->link = tokb;

  ASSIGNtok->operands = temp2;
  temp2->link = PLUStok;
  
  PLUStok->operands = temp3;
  temp3->link = onetok;

  if(DEBUG & DB_MAKEFOR) {
    printf("makefor\n");
    dbugprinttok(iftok);
    dbugprinttok(progntok);
    dbugprinttok(onetok);

    dbugprinttok(temp1);
    dbugprinttok(temp2);
    dbugprinttok(temp3);

    dbugprinttok(ASSIGNtok);
    dbugprinttok(LEtok);
    dbugprinttok(PLUStok);
  }
  return tok;
}

main()
  { int res;
    initsyms();
    res = yyparse();
    printst();
    printf("yyparse result = %8d\n", res);
    if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
    ppexpr(parseresult);           /* Pretty-print the result tree */
  }