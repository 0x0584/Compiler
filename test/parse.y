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
#include <stdlib.h>
#include <string.h>
#include <float.h>

#include "token.h"
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

  program     : PROGRAM IDENTIFIER LPAREN IDENTIFIER RPAREN SEMICOLON 
                labelBLK DOT { parseresult = makeprogram($2, $4, $7); }
              ;
  getID      : IDENTIFIER COMMA getID     { $$ = cons($1, $3); }
              | IDENTIFIER                  { $$ = cons($1, NULL); }
              ;
  labelBLK    : LABEL lnum SEMICOLON constBLK    { $$ = $4; } 
              | constBLK                         
              ;
  constBLK    : CONST constid typeBLK        { $$ = $3; }
              | typeBLK                     
              ;
  typeBLK     : TYPE htype  varBLK        { $$ = $3; }
              | varBLK                    
              ;
  varBLK      : VAR varsemi statement        { $$ = $3; }
              | statement                     
              ;
  lnum        : NUMBER COMMA lnum        { instlabel($1); }
              | NUMBER              { instlabel($1); } 
              ;
  constid     : IDENTIFIER EQ NUMBER SEMICOLON constid         { instconst($1, $3); }
              | IDENTIFIER EQ NUMBER SEMICOLON  { instconst($1, $3); }
              ;
  htype       : idtype SEMICOLON htype      { $$ = $3; }
              | idtype SEMICOLON            
              ; 
  idtype      : IDENTIFIER EQ type        { insttype($1, $3); }
              ;
  varsemi     : varcol SEMICOLON varsemi   
              | varcol SEMICOLON        
              ;
  varcol      : getID COLON type         { instvars($1, $3); }
              ;
//-----------------------------------------------------------------------------------
  stmtlist    : statement SEMICOLON stmtlist    { $$ = cons($1, $3); }
              | statement                       
              ;
  type        : simpletype                 { $$ = $1; }
              | POINT IDENTIFIER          { $$ = instpoint($1, $2); }
              | ARRAY LBRACKET simplelist RBRACKET OF type        { $$ = instarray($3, $6); }
              | RECORD fieldlist END        { $$ = instrec($1, $2); }
              ;
  simpletype  : IDENTIFIER                  { $$ = findtype($1); }
              | LPAREN getID RPAREN        { $$ = instenum($2); }
              | NUMBER DOTDOT NUMBER        { $$ = instdotdot($1, $2, $3); }
              ;
  simplelist  : simpletype COMMA simplelist   { $$ = cons($3, $1); } 
              | simpletype            { $$ = $1; }
              ;
  loop        : FOR assignment TO expr DO statement        { $$ = makefor(1, $1, $2, $3, $4, $5, $6); }
              | IF expr THEN statement endif               { $$ = makeif($1, $2, $4, $5); }
              | IDENTIFIER LPAREN argslist RPAREN          { $$ = makefuncall($2, $1, $3); } 
              | WHILE expr DO statement                    { $$ = makewhile($1, $2, $3, $4); }
              | REPEAT stmtlist UNTIL expr                 { $$ = makerepeat($1, $2, $3, $4); }
              | GOTO NUMBER                                { $$ = dogoto($1, $2); }
              | call
              ;
  call        : type LPAREN expr RPAREN  { $$ = makefuncall(talloc(), findid($1), $3); }
              ; 
  argslist    : expr COMMA argslist       { $$ = cons($1, $3); }
              | expr                       { $$ = cons($1, NULL); }
              ;
//------------------------------------------------------------------------------------        
  statement   : BEGINBEGIN statement endpart               { $$ = makeprogn($1, cons($2, $3)); }
              | NUMBER COLON statement                   { $$ = dolabel($1, $2, $3); }
              | assignment        
              | loop  
              ;
    endpart   : SEMICOLON statement endpart     { $$ = cons($2, $3); }
              | END                             { $$ = NULL; }
              ;    
    endif     : ELSE statement                  { $$ = $2; }
              |                                 { $$ = NULL;}
              ;
  assignment  : factor ASSIGN expr            { $$ = binop($2, $1, $3); }
              ;
    var       : IDENTIFIER                 { $$ = findid($1); }
              | var DOT IDENTIFIER         { $$ = reducedot($1, $2, $3); }
              | var amerge                { $$ = arrayref($1, NULL, $2, NULL); }
              | var POINT                  { $$ = dopoint($1, $2); }
              ;
  amerge      : LBRACKET argslist RBRACKET             { $$ = $2; }
              | LBRACKET argslist RBRACKET amerge      { $$ = nconc($2, $4); }
              ;
  fieldlist   : getID COLON type      { instfields($1, $3); }
              | getID COLON type SEMICOLON fieldlist       { instfields($1, $3); $$ = nconc($1, $5); }
              ;
  expr        : IDENTIFIER LT NUMBER       { findid($1); $$ = binop($2, $1, $3);}
              | IDENTIFIER EQ expr         { findid($1); $$ = binop($2, $1, $3);}
              | term PLUS term             { $$ = binop($2, $1, $3); }
              | MINUS term                 { $$ = unaryop($1, $2);}
              | term MINUS term            { $$ = binop($2, $1, $3);}
              | term NE term               { $$ = binop($2, $1, $3);} 
              | STRING                    
              | term 
              ;
  term        : factor TIMES factor               { $$ = binop($2, $1, $3); }
              | factor              
              ;   
  factor      : LPAREN expr RPAREN              { $$ = $2; }        
              | var               
              | call
              | NOT factor            { $$ = unaryop($1, $2); }
              | NUMBER  
              | STRING            
              | NIL               
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

//removed most printouts

 int labelnumber = 0;  /* sequential counter for internal label numbers */

/* dopoint handles a ^ operator.
   tok is a (now) unused token that is recycled. */
TOKEN dopoint(TOKEN var, TOKEN tok) {
  tok->operands = var;
  return tok;
}

/* instdotdot installs a .. subrange in the symbol table.
   dottok is a (now) unused token that is recycled. */
TOKEN instdotdot(TOKEN lowtok, TOKEN dottok, TOKEN hightok) {
  int a = lowtok->intval;
  int b = hightok->intval;

  return makesubrange(dottok, a, b);
}

/* instenum installs an enumerated subrange in the symbol table,
   e.g., type color = (red, white, blue)
   by calling makesubrange and returning the token it returns. */
TOKEN instenum(TOKEN idlist) {
  TOKEN tok = idlist;
  TOKEN subrange = talloc();
  int size = 0;

  for (size; tok ; size++) {
    instconst(tok, makeintc(size));
    tok = tok->link;
  }

  size--;
  subrange = makesubrange(idlist, 0, size);
  return subrange;
}

/* instfields will install type in a list idlist of field name tokens:
   re, im: real    put the pointer to REAL in the RE, IM tokens.
   typetok is a token whose symtype is a symbol table pointer.
   Note that nconc() can be used to combine these lists after instrec() */
TOKEN instfields(TOKEN idlist, TOKEN typetok) {
  TOKEN tok = idlist;
  SYMBOL record = makesym(tok->stringval);
  SYMBOL type = searchst(typetok->stringval);

  int jump = 0;
  int offnum = 0;

  while (tok) {
    offnum = jump;
    jump = jump + type->size;

    type = searchst(typetok->stringval);
    record = makesym(tok->stringval);
    record->datatype = type;
    record->offset = offnum;
    record->size = type->size;

    if (type->kind == BASICTYPE) {
      record->basicdt = type->basicdt;
    }

    tok->symtype = record;
    tok = tok->link;
  }

  return tok;
}


 /* instpoint will install a pointer type in symbol table */
TOKEN instpoint(TOKEN tok, TOKEN typename){
  SYMBOL sym = insertsym(typename->stringval);
  SYMBOL psym = symalloc();   

  sym->kind = TYPESYM;
  
  psym->kind = POINTERSYM;
  psym->size = basicsizes[POINTER];
  psym->basicdt = POINTER;
  psym->datatype = sym;

  tok->symtype = psym;
  return tok;
}

/* instrec will install a record definition.  Each token in the linked list
   argstok has a pointer its type.  rectok is just a trash token to be
   used to return the result in its symtype */
TOKEN instrec(TOKEN rectok, TOKEN argstok) {
  TOKEN second = argstok->link;
  TOKEN first = argstok;

  SYMBOL recsym = symalloc();

  int offset = wordaddress(argstok->symtype->size, 8);
  int offset_size = offset;

  while (second) {
    first->symtype->link = second->symtype;
    second->symtype->offset = offset_size;

    offset = wordaddress(second->symtype->size, 8);   
    offset_size += offset;

    first = second;
    second = second->link;
  }

  recsym->kind = RECORDSYM;
  rectok->symtype = recsym;
  recsym->datatype = argstok->symtype;
  recsym->size = offset_size;

  return rectok;
}

/* insttype will install a type name in symbol table.
   typetok is a token containing symbol table pointers. */
void insttype(TOKEN typename, TOKEN typetok) {
  SYMBOL tempsym = searchins(typename->stringval);
  SYMBOL type = typetok->symtype;

  tempsym->kind = TYPESYM;
  tempsym->basicdt = type->basicdt;
  tempsym->size = type->size;
  tempsym->datatype = type;
}

/* makesubrange makes a SUBRANGE symbol table entry, puts the pointer to it
   into tok, and returns tok. */
TOKEN makesubrange(TOKEN tok, int low, int high){

  SYMBOL sub = makesym("sub");
  sub->kind = SUBRANGE;
  sub->lowbound = low;
  sub->highbound = high;

  sub->basicdt = INTEGER;
  sub->size = basicsizes[INTEGER];

  tok->symtype = sub;
}

/* nconc concatenates two token lists, destructively, by making the last link
   of lista point to listb.
   (nconc '(a b) '(c d e))  =  (a b c d e)  */
/* nconc is useful for putting together two fieldlist groups to
   make them into a single list in a record declaration. */
TOKEN nconc(TOKEN lista, TOKEN listb) {
  //can use get_last to immediatly resolve nconc
  TOKEN tok = get_last(lista, 0);
  tok->link = listb;

  return lista;
}

/* reducedot handles a record reference.
   dot is a (now) unused token that is recycled. */
TOKEN reducedot(TOKEN var, TOKEN dot, TOKEN field) {
  TOKEN areftok;
  TOKEN offtok = makeintc(-1);
  TOKEN lasttok = get_last(var->operands, 0);

  SYMBOL locate = symalloc();
  SYMBOL varsym = searchst(get_last(var, 1)->stringval);
  SYMBOL temp = varsym;

  int done = 0;

  areftok = makeop(AREFOP);
  areftok->operands = var;
  var->link = offtok;

  while (temp && !done) {
    if (temp->datatype && temp->datatype->kind == BASICTYPE) 
      done = 1;
    if (!done)
      temp = temp->datatype;
  }
  done = 0;
  while (temp && !done) {
    if ((strcmp(temp->namestring, field->stringval) == 0)) {
      locate = temp;
      offtok->whichval = -1;
      if (var->whichval == AREFOP) 
          offtok->whichval = lasttok->whichval + locate->offset; 
      else {
        offtok->whichval = locate->offset;
        if (locate->datatype->basicdt == REAL) {
          offtok->link = makerealtok();
        }
      }
    if (temp->offset == lasttok->whichval) {
      locate = temp;
      done = 1;
      }
    }
    temp = temp->link;
  }

  if (var->whichval == AREFOP) {
    if (offtok->whichval >= 0) {
      var->operands->link = offtok;
    }
    areftok = var;
  }

  if (offtok->link && offtok->link->datatype == REAL) 
    areftok->datatype = REAL;
  
  offtok->link = NULL;

  return areftok;
}

/* addint adds integer off to expression exp, possibly using tok */
TOKEN addint(TOKEN exp, TOKEN off, TOKEN tok) {

  int exp_int = exp->intval;
  int off_int = off->intval;
  exp->intval = exp_int + off_int;
  
  return exp;
}

/* Gets and returns the last TOKEN (in)directly
   connected via ->operands or ->links to TOKEN tok. */
TOKEN get_last(TOKEN tok, int op) {
  TOKEN current = tok->link;
  if (op)
    current = tok->operands;
  while (current) {
    tok = current;
    if (op)
      current = current->operands;
    else
      current = current->link;
  }
  
  return tok;  
}

/* instlabel installs a user label into the label table */
void instlabel (TOKEN num) {
  int index = labelnumber++;
  all_labels[index] = num->intval;
  }


/* makefloat forces the item tok to be floating, by floating a constant
   or by inserting a FLOATOP operator */
TOKEN makefloat(TOKEN tok) {
  TOKEN flaottok = tok;

  if (tok->tokentype == NUMBERTOK) {
    flaottok->datatype = REAL;
    flaottok->realval = flaottok->intval;
  }
  else {
    flaottok = makeop(FLOATOP);
    flaottok->operands = tok;
  }
  return flaottok;
}

/* simple function to make a REAL tok */
TOKEN makerealtok() {
  TOKEN temp = talloc();
  temp->tokentype = NUMBERTOK;
  temp->datatype = REAL;
  return temp;
}

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { item->link = list;

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

/* binop links a binary operator op to two operands, lhs and rhs */
TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs) {
  op->operands = lhs;   // link operands to operator
  lhs->link = rhs;    // link second operand to first
  rhs->link = NULL;   // terminate operand list
  op->datatype = lhs->datatype;

  if (lhs->datatype != rhs->datatype) {
    TOKEN temp = makefloat(rhs);
    if (lhs->datatype == REAL && rhs->datatype == INTEGER) {
      op->datatype = REAL;
      op->operands = lhs;
      lhs->link = temp;
    }
    else if (lhs->datatype == INTEGER && rhs->datatype == REAL) {
      op->datatype = REAL;
      temp = makefloat(lhs);
      op->operands = temp;
      temp->link = rhs;

      if (op->whichval == ASSIGNOP) {
        temp = makeop(FIXOP);
        op->operands = lhs;
        lhs->link = temp;
      }
    }
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
  TOKEN tempLabel = makelabel();
  TOKEN tokProg = makeprogn(talloc(), tempLabel); // operand tempLabel to tokProg
  TOKEN tokGOTO = makegoto(tempLabel->operands->intval);
  TOKEN tokProgOP = makeop(PROGNOP);
  TOKEN tokIF = makeif(makeop(IFOP), expr, tokProgOP, NULL);

  tokProgOP->link = tokGOTO;

  tempLabel->link = statements;

  //fail to allocate
  if (!tempLabel || !tokProg || !tokGOTO || !tokProgOP ||!tokIF) 
    return NULL;
  
//get nested progns 
  get_last(statements, 0)->link = tokIF;

  return tokProg;
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
TOKEN makeprogn(TOKEN tok, TOKEN statements) { 
  //if statements is already a progn
  if (statements->whichval == PROGNOP) 
    return statements;

   tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;

     return tok;
   }

/* findid finds an identifier in the symbol table and sets up
the symbol table pointers */
/* taken from lecture notes pg 127 */
TOKEN findid(TOKEN tok) {
  SYMBOL sym, typ;
  sym = searchst(tok->stringval);
  if (sym->kind == CONSTSYM) {
    if (sym->basicdt == INTEGER) {
      tok->tokentype = NUMBERTOK;
      tok->datatype = INTEGER;
      tok->intval = sym->constval.intnum;
    }
    if (sym->basicdt == REAL) {
      tok->tokentype = NUMBERTOK;
      tok->datatype = REAL;
      tok->realval = sym->constval.realnum;
    }
  }
  else {
    tok->symentry = sym;
    typ = sym->datatype;
    tok->symtype = typ;

    if (typ->kind == BASICTYPE || typ->kind == POINTERSYM) {
      tok->datatype = typ->basicdt;
    }
  }

  return tok;
}

/* findtype looks up a type name in the symbol table, puts the pointer
   to its type into tok->symtype, returns tok. */
TOKEN findtype(TOKEN tok) {
  SYMBOL sym;
  sym = searchst(tok->stringval);
  tok->symtype = sym->datatype;

  if (sym->kind == BASICTYPE) {
    tok->datatype = sym->basicdt;
    tok->symtype = sym;
  }

  return tok;
}


/* instconst installs a constant in the symbol table */
void instconst(TOKEN idtok, TOKEN consttok) {
  SYMBOL sym;

  sym = insertsym(idtok->stringval);
  sym->kind = CONSTSYM;
  sym->basicdt = consttok->datatype;

  if (sym->basicdt == REAL) //real
    sym->constval.realnum = consttok->realval;

  if (sym->basicdt == INTEGER) //int
    sym->constval.intnum = consttok->intval;

  else if (sym->basicdt == STRINGTYPE) //string
    strncpy(sym->constval.stringconst, consttok->stringval, 16);
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
TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args) {
  TOKEN functok = copytok(tok);
  TOKEN memtok = copytok(tok);

  fn->link = args;
  functok->operands = fn;

  if(!(strcmp(fn->stringval, "writeln")) || !(strcmp(fn->stringval, "write"))) {
    SYMBOL writelnf = searchst("writelnf");
    if(args->tokentype == OPERATOR)
      strcpy(fn->stringval, "writelnf");
    SYMBOL writelni = searchst("writelni");
    if(args->tokentype == IDENTIFIERTOK)
      strcpy(fn->stringval, "writelni");
  }

  SYMBOL func_symbol = searchst(fn->stringval);

  functok->tokentype = OPERATOR;
  functok->datatype = STRINGTYPE;
  functok->whichval = FUNCALLOP;
  functok->link = NULL;

  if(strcmp(fn->stringval, "new") == 0){
    //create number token for size of allocated memory
    memtok->tokentype = NUMBERTOK;
    memtok->datatype = INTEGER;
    memtok->symtype = searchst("integer");
    memtok->intval = searchst(args->stringval)->datatype->datatype->datatype->size;

    fn->link = memtok;
    functok->operands = fn;

    //create an assignop
    TOKEN assigntok = copytok(memtok);
    assigntok->whichval = ASSIGNOP;
    assigntok->tokentype = OPERATOR;
    functok->symentry = func_symbol;
    
    return binop(assigntok, args, functok);
    }
  
    //search symbol table for function
    int return_type = func_symbol->datatype->basicdt;
    int arg_type = func_symbol->datatype->link->basicdt;

  return functok;
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
  TOKEN tok_goto = talloc();
  tok_goto->operands = makeintc(label);
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

/* arrayref processes an array reference a[i]
   subs is a list of subscript expressions.
   tok and tokb are (now) unused tokens that are recycled. */
TOKEN arrayref(TOKEN arr, TOKEN tok, TOKEN subs, TOKEN tokb){
  SYMBOL symArray = searchst(arr->stringval);
  SYMBOL symType = symArray->datatype->datatype->datatype;

  TOKEN subscipt = talloc();;
  TOKEN areftok = talloc();
  TOKEN arrSize = talloc();
  TOKEN arrcpy = talloc();

  TOKEN timetok = talloc();
  TOKEN timecpy = talloc();

  int dub = 0;
  int offset = 0;

  if(subs->link) {
    symType = symArray->datatype->datatype; 
    dub = 1;
  }

  if(subs->tokentype == IDENTIFIERTOK){
    timetok->tokentype = OPERATOR;
    timetok->whichval = TIMESOP;
    timecpy = copytok(timetok);
    timecpy->whichval = PLUSOP;

    arrSize->tokentype = NUMBERTOK;
    arrSize->datatype = INTEGER;
    arrSize->intval = symType->size;
    arrcpy = copytok(arrSize);

    subscipt = binop(timetok, arrSize, subs);

    if(!dub)
          arrcpy->intval = -arrcpy->intval;
    else
      arrcpy->intval = -arrcpy->intval + 4;     
    subscipt = binop(timecpy, arrcpy, subscipt);
    dub = 1;
  } 

  areftok->tokentype = OPERATOR;
  areftok->whichval = AREFOP;
  areftok->operands = arr;
  
  if(dub)
    arr->link = subscipt;
  else{
    offset = symType->size * (subs->intval - 1);
    subs->intval = offset;
    arr->link = subs;
  }
  return areftok;
}

/* makefor makes structures for a for statement.
   sign is 1 for normal loop, -1 for downto.
   asg is an assignment statement, e.g. (:= i 1)
   endexpr is the end expression
   tok, tokb and tokc are (now) unused tokens that are recycled. */
TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr,
              TOKEN tokc, TOKEN statement) {
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

/* makewhile makes structures for a while statement.
   tok and tokb are (now) unused tokens that are recycled. */
TOKEN makewhile(TOKEN tok, TOKEN expr, TOKEN tokb, TOKEN statement){
  TOKEN new_label = talloc();
  TOKEN numtok = talloc();
  TOKEN iftok = talloc();
  TOKEN linktok = unaryop(iftok, expr);
  TOKEN temptok = talloc();
  TOKEN gototok = talloc();

  labelnumber++;
  new_label->tokentype = OPERATOR;
  new_label->whichval = LABELOP;

  //create numtok
  numtok->tokentype = NUMBERTOK;
  numtok->intval = labelnumber;
  new_label->operands = numtok;

  //if section
  iftok->tokentype = OPERATOR;
  iftok->whichval = IFOP;
  new_label->link = iftok;
  linktok->operands->link = statement;

  //link goto with get_last
  temptok = get_last(statement->operands, 0);
  
  //create goto
  gototok->tokentype = OPERATOR;
  gototok->whichval = GOTOOP;
  temptok->link = unaryop(gototok, numtok);

  return makeprogn(tok, new_label);
}

/* instarray installs an array declaration into the symbol table.
   bounds points to a SUBRANGE symbol table entry.
   The symbol table pointer is returned in token typetok. */
TOKEN instarray(TOKEN bounds, TOKEN typetok) {
  TOKEN type = typetok;
  SYMBOL prev_sym = NULL;
  SYMBOL typesym = searchst(type->stringval);
  SYMBOL arrsym = symalloc();
  int low = 0;
  int high = 0;
  int count = 0;
  int size = 0;

  for(count; bounds; count++) {
    low = bounds->symtype->lowbound;
    high = bounds->symtype->highbound;

    arrsym = symalloc();
    arrsym->kind = ARRAYSYM;
    arrsym->datatype = typesym;
    if (count) {
      arrsym->datatype = type->symtype;
      arrsym->size = (high - low + 1) * size;
      }

    else 
      arrsym->size = (high - low + 1) * typesym->size;

    arrsym->lowbound = low;
    arrsym->highbound = high;
    type->symtype = arrsym;
    prev_sym = arrsym;
    size = prev_sym->size;
    bounds = bounds->link;
  }
  
  return type;
}

/* dogoto is the action for a goto statement.
   tok is a (now) unused token that is recycled. */
TOKEN dogoto(TOKEN tok, TOKEN labeltok) {
  TOKEN gotoTok = talloc();
  TOKEN numtok = talloc();
  int index;

  gotoTok->tokentype = OPERATOR;
  gotoTok->whichval = GOTOOP;
  numtok->tokentype = NUMBERTOK;

  //incrementing index
  for (index = 0; all_labels[index] != labeltok->intval; index++){}
  
  numtok->intval = index;
  gotoTok->operands = numtok;

  return gotoTok;
}

/* dolabel is the action for a label of the form   <number>: <statement>
   tok is a (now) unused token that is recycled. */
TOKEN dolabel(TOKEN labeltok, TOKEN tok, TOKEN statement) {
  TOKEN new_label = talloc();
  TOKEN progn = makeprogn(tok, statement);
  int index;

  new_label->tokentype = OPERATOR;
  new_label->whichval = LABELOP;
  new_label->operands = labeltok;
  new_label->link = statement;

  //incrementing index
  for(index = 0; all_labels[index] != labeltok->intval; index++){}

  labeltok->intval = index;
  progn->operands = new_label;
  
  return progn;
}

int main(int argc, char **argv) {
  int res;
  initsyms();
  res = yyparse();
  printst();
  printf("yyparse result = %8d\n", res);
  if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
  ppexpr(parseresult);           /* Pretty-print the result tree */
}
