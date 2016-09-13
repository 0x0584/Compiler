//John Thomas jtt767

/* lexanc.c         14 Feb 01; 31 May 12       */

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

//lists of reserved operators, delimiters and reserve words
char *operators[] = {"+", "-", "*", "/", ":=", "=", "<>", "<", "<=",
 ">=", ">", "^", ".", "and", "or", "not", "div", "mod", "in"};

char *dilimiters[] = {",", ";", ":", "(", ")", "[", "]", ".."};

char *reserve[] = {"array", "begin", "case", "const", "do", "downto",
 "else", "end", "file", "for", "function", "goto", "if", "label", "nil",
 "of", "packed", "procedure", "program", "record", "repeat", "set", "then",
  "to", "type", "until", "var", "while", "with"};

//negative decimal conversion list and initialization flag
double neg_dec[80];
int neg_init = 0;

//positive decimal conversion list and initialization flag
double pos_dec[80]; 
int pos_init = 0;

/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks () {  
    int c;
    int cc;
    int comp;
    int done = 0;

    while ((c = peekchar()) != EOF && (c == ' ' || c == '\n' || c == '\t' ||
    c == '{' || (c == '(' && ((cc = peek2char()) == '*')))){
      //handeling { } comment type
      if(c == '{')
        while(c = peekchar() != '}') 
          getchar();
      //handeling (* *) comment type
      if(c == '(' && cc == '*'){
          getchar();
        while(!done) {
          getchar();
          if(peek2char() == ')' && (peekchar() == '*')){
            getchar();
            done = 1;
          }
          comp = 1;
        }
        done = 0;
      }
      //move past * at end of (* *)
      if(comp)
        getchar();
    }
  }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok) { 
    char name[15];
    char c = peekchar();
    int count = 0, which;

    //check identifier begins with a letter
    if(c != EOF && CHARCLASS[c] == ALPHA) {
        c = getchar();
        name[count] = c;
        count++;
      }
    else
        return tok;
    while((c = peekchar()) != EOF && (CHARCLASS[c] == ALPHA || CHARCLASS[c] == NUMERIC)) {
      c = getchar();
      //check to see if we already have 15 identifier characters
      if (count <= 15) {
      name[count] = c;
      count++;
    }
      c = peekchar();
    }
    name[15] = 0;
    name[count] = 0;
    strcpy(tok->stringval, name);
    tok->datatype = STRINGTYPE;

    if((which = check(name, 0)) >= 0){
      //check if reserved word
      tok->tokentype = RESERVED;
      tok->whichval = which;
    }
    else if((which = check(name, 1)) >= 0){
      //check if operator reserve word
      tok->tokentype = OPERATOR;
      tok->whichval = which;
    }
    else{
      //identifier is normal
      tok->tokentype = IDENTIFIERTOK;
    }
  }

/*get potental 15 character string*/
TOKEN getstring (TOKEN tok) {
    char word[15];
    int index = 0, done = 0;
    getchar();
    int c;
    tok->tokentype = STRINGTOK;
    tok->datatype = STRINGTYPE;
    while(!done){
      c = getchar();
      if(peekchar() != '\'') {
          if(index < 15) {
            word[index] = c;
            index++;
            }
        }
      else if(peekchar() == '\''){
        if(peek2char() == '\''){
          getchar();
          word[index] = c;
          index++;
        }
        else {
          getchar();
          if(index < 15) {
            word[index + 1] = 0;
            word[index] = c;
          }
          done = 1;
        }
        word[15] = 0;
      }
    }
    strcpy(tok->stringval, word);
  }

/*get special characters*/
TOKEN special (TOKEN tok) {
  int c, cc;
  int which;
  char spec[3];
	c = getchar();
	cc = peekchar();
	if(cc != '>' && cc != '=' && cc != '.') {
		spec[0] = c;
		spec[1] = 0;
	}
	else{
		cc = getchar();
		spec[2] = 0;
		spec[1] = cc;
		spec[0] = c;
	//	printf("spec[0] = %c, spec[1] = %c\n", spec[0] ,spec[1]);
	}
  if((which = check(spec, 1)) >= 0){
   // printf("is an op %d\n", which);
    tok->tokentype = OPERATOR;
    tok->whichval = which;
  }
  else if((which = check(spec, 2)) >= 0) {
     // printf("is a delim %d\n", which);
      tok->tokentype = DELIMITER;
      tok->whichval = which;
    }
  }

/* Get and convert unsigned numbers of all types. */
/*each number is split into 3 main parts :
  1) the part leading up to the decimal point or e
  2) the part between the decimal point and e
  3) everything after the e
these sections are handled with calls to check_number and check_decimal */
TOKEN number (TOKEN tok) { 
    int  c, charval;
    double double_charval;

    int real_flag = 0; // set to to 1 if number is a double
    int exp_flag = 0; // set to 1 if exponent is not between -38 and 38
    int neg_num = 0; // set to 1 if number is negative
    int zero_flag = 0; // set to 1 if number is not all zeros

    long long_num = 0; 
    double double_num = 0; 
    int exponent_num = 0;
    int sig_dig = 0;
    int first_sig = 0;  
    int i;

    //creates decimal conversions
    if(!neg_init) 
      create_table(1);
      neg_init = 1;
    if(!pos_init) 
      create_table(0);
      pos_init = 1;

    //check all numbers up to decimal or e
    check_number(0, &charval, &c, &zero_flag, &sig_dig, &exponent_num, 
      &long_num, &double_num);

    //check if our number has a decimal
    check_decimal(&c, &zero_flag, &sig_dig, &double_charval, &real_flag, 
      &double_num, &first_sig);
        
    // get exponent if present
    if((c = peekchar()) == 'E' || c == 'e') {
      real_flag = 1;
      getchar(); 
      sig_dig = 0;
      zero_flag = 0;
      
      //get the sign character if there is one
      c = peekchar();
      if(c == '-') {
        neg_num = 1;
          getchar();
      }
      if(c == '+')
          getchar();

      //get exponent value if there is one
      check_number(1, &charval, &c, &zero_flag, &sig_dig, &exponent_num, NULL, NULL);
      //exponent value
      if(exponent_num < 0)
          exp_flag = 1;
      if(neg_num)
          exponent_num = -exponent_num;
    }

    // final error checking and token assignment
    error_checker(tok, real_flag, exponent_num, double_num, sig_dig, 
      long_num, first_sig, exp_flag);
    return (tok);
  }

//helper method to compare input to operators, delimiters and reserved words
int check(char * arg, int opp) {
  int index = 0;
  int size = 28;
  char ** look = reserve;
  if(opp == 1) {
    size = 19;
    look = operators;
  }
  else if(opp == 2){
    size = 8;
    look = dilimiters;
  }
  for(index; index < size; index++) 
    if(strcmp(arg, look[index]) == 0) 
        return (index + 1);
  return -1;
}

//helper method for number 
//checks for exponent if expontent parameter = 1
//loops through all numbers before decimal or exponent if 0
int check_number(int exponent, int *charval, int *c, int *zero_flag, 
  int *sig_dig, int *exponent_num, long *long_num, double *double_num) {
      while ((*c = peekchar()) != EOF && CHARCLASS[*c] == NUMERIC) {  
      *c = getchar();
      *charval = (*c - '0');
      if(*charval != 0)
        *zero_flag = 1;
      if (!exponent)
        *long_num = *long_num * 10 + *charval;
       if(*zero_flag == 1){
          if(!exponent) {
            if(*sig_dig >= 8)
             *charval = 0;
           *double_num = (*double_num * 10) + *charval;
          }
           (*sig_dig)++;  
           if (exponent)
            *exponent_num = *exponent_num * 10 + *charval;
         }
       }
 }

//helper method for number to check if our number has a decimal and set flags accordingly
int check_decimal(int *c, int *zero_flag, int *sig_dig, double *double_charval, 
  int *real_flag, double *double_num, int *first_sig) {
  int temp;
  *first_sig = *sig_dig  - 1;
  if((*c = peekchar()) == '.' && CHARCLASS[*c = peek2char()] == NUMERIC) {
    int dec_count = 1;
    *real_flag = 1;
    *c = getchar();
    while ((*c = peekchar()) != EOF && CHARCLASS[*c] == NUMERIC) {  
        *c = getchar();
        *double_charval = (*c - '0');
        *double_charval = *double_charval * neg_dec[dec_count++];
        if(*double_charval != 0)
            *zero_flag = 1;
        if(*zero_flag == 0)
               (*first_sig)--;
        if(*zero_flag == 1 && *sig_dig  < 8){
            (*sig_dig)++;
            *double_num = *double_num + *double_charval;
        }
    }
  }
  temp = *first_sig * -1;
  //need to move significant digits
  if(*first_sig < 0)
      *double_num = *double_num * pos_dec[temp];
  if(*first_sig > 0)
      *double_num = *double_num * neg_dec[temp * -1];
 }

//helper method for number to error check funal values and set tok types
TOKEN error_checker(TOKEN tok, int real_flag, int exponent_num, 
  double double_num, int sig_dig, long long_num, int first_sig, int exp_flag){
  int i;
  tok->tokentype = NUMBERTOK;
  if(real_flag == 1){
    tok->datatype = REAL;
    exponent_num += first_sig;
    if(exponent_num > 38 || exponent_num < -38 ||  exp_flag == 1) {
        double_num = 0;
        printer(1);
    }
    else{
        if(exponent_num > 0)
            double_num = double_num * pos_dec[exponent_num];
        else if(exponent_num < 0){
            for(i = 0; i > exponent_num; i--)
                double_num = double_num * 0.1;
        }
    }
    tok->realval = double_num;
  }
  else {
    tok->datatype = INTEGER;
    if(long_num > 2147483647 || sig_dig  > 10 || long_num < 0) {
       //need to truncate integer out of range
       // printf("run 3 : long_num = %ld double_num = %f\n", long_num, double_num);
       int truncate = sig_dig - 9;
       for(i = 1; i <= truncate; i++)
          long_num = long_num / 10;
          printer(0);
      }
      tok->intval = long_num;
      long_num = 0;
  }
  return tok;
 }

int create_table(int neg_table) {
  int i;
  double mult;
  if (neg_table) {
    neg_dec[0] = 1;
    mult = 0.1;
  }
  else {
    pos_dec[0] = 1;
    mult = 10;
  }
  for (i = 1; i < 80; i++){
    if (neg_table) 
      neg_dec[i] = (neg_dec[i - 1] * mult);
    else
      pos_dec[i] = (mult * pos_dec[i - 1]);
  }
}

//error printer method
void printer(int floating) {
  if (floating)
    printf("Floating number out of range\n");
  else
    printf("Integer number out of range\n");
}