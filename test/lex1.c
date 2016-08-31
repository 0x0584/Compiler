/* lex1.c         14 Feb 01; 31 May 12       */

/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */

/* Copyright (c) 2001 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "lexan.h"

char *operators[] = {"+", "-", "*", "/", ":=", "=", "<>", "<", "<=",
 ">=", ">", "^", ".", "and", "or", "not", "div", "mod", "in"};

 char *dilimiters[] = {",", ";", ":", "(", ")", "[", "]", ".."};

char *reserve[] = {"array", "begin", "case", "const", "do", "downto",
 "else", "end", "file", "for", "function", "goto", "if", "label", "nil",
 "of", "packed", "procedure", "program", "record", "repeat", "set", "then"
 "to", "type", "until", "var", "while", "with"};

/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
   */

/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks ()
  {  int c;
     int cc;
     int comp;

    while ((c = peekchar()) != EOF && (c == ' ' || c == '\n' || c == '\t' ||
      c == '{' || (c == '(' && ((cc = peek2char()) == '*')))){
       //handeling { } comment type
	     if(c == '{')
        while(c = peekchar() != '}') 
			      getchar();
       //handeling (* *) comment type	
		   else if(c == '(' && cc == '*'){
         getchar();
		     while(cc = peek2char() != ')' && (c = peekchar() != '*')) 
		     	getchar();
	     	comp = 1;
	      }
        //move past * at end of (* *)
        if(comp)
           getchar();
         getchar();
	    }
    }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
  { char name[15];
    char c = peekchar();
    int count = 0;
    int word;

    //check identifier begins with a letter
    if(c != EOF && CHARCLASS[c] == ALPHA) {
        c = getchar();
        name[count] = c;
        count++;
      }
    else
        return tok;

    while((c = peekchar()) != EOF && (count <= 15) && (CHARCLASS[c] == ALPHA || CHARCLASS[c] == NUMERIC)) {
      c = getchar();
      name[count] = c;
      count++;
      c = peekchar();
    }
    name[15] = 0;
    name[count] = 0;
    strcpy(tok->stringval, name);
    //check if reserved word
    word = check(name, 0);
    if(word >= 0) {
      tok->tokentype = RESERVED;
      tok->whichval = word;
    }
    else
      word = check(name, 1);

  

    }

TOKEN getstring (TOKEN tok)
  {
    }

TOKEN special (TOKEN tok)
  {
    }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { long num;
    int  c, charval;
    num = 0;
    while ( (c = peekchar()) != EOF
            && CHARCLASS[c] == NUMERIC)
      {   c = getchar();
          charval = (c - '0');
          num = num * 10 + charval;
        }
    tok->tokentype = NUMBERTOK;
    tok->datatype = INTEGER;
    tok->intval = num;
    return (tok);
  }

//helper method to compare identifier to reserved words
int check (char * arg, int opp) {
  int index = 0;
  int size = 28;                    
  char ** look = reserve;
  if(opp) {
    index = 13;
    size = 18;
    look = operators;
  }
  for(index; index < size; index++)
    if(strcmp(arg, look[index]) == 0)
        return (index + 1);
  return -1;
}