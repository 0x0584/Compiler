//John Thomas jtt767
//added declarations of helper functions

/* lexan.h            Gordon S. Novak Jr.           09 Feb 01 */

/* Definitions for lexical analyzer */

/* Copyright (c) 2001 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License (file gpl.text) for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#define MAXCHARCLASS 256

#define ALPHA   1
#define NUMERIC 2
#define SPECIAL 3

#define DEBUGGETTOKEN 0

TOKEN talloc();
int peekchar();
int peek2char();
void init_charclass();
TOKEN gettoken();
void printtoken(TOKEN tok);

void skipblanks ();
void initscanner ();
int EOFFLG;
int CHARCLASS[MAXCHARCLASS];
TOKEN identifier (TOKEN tok);
TOKEN getstring (TOKEN tok);
TOKEN special (TOKEN tok);
TOKEN number (TOKEN tok);
int check (char *arg, int opp);
int check_number(int exponent, int *charval, int *c, int *zero_flag, 
  int *sig_dig, int *exponent_num, long *long_num, double *double_num);
int check_decimal(int *c, int *zero_flag, int *sig_dig, double *double_charval, 
  int *real_flag, double *double_num, int *first_sig);
TOKEN error_checker(TOKEN tok, int real_flag, int exponent_num, 
  double double_num, int sig_dig, long long_num, int first_sig, int exp_flag);
int create_table(int neg_table);
void printer(int print);