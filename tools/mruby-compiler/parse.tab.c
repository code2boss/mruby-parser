/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 7 "parse.y" /* yacc.c:339  */

#undef PARSER_DEBUG
#ifdef PARSER_DEBUG
# define YYDEBUG 1
#endif
#define YYERROR_VERBOSE 1
/*
 * Force yacc to use our memory management.  This is a little evil because
 * the macros assume that "parser_state *p" is in scope
 */
#define YYMALLOC(n)    mrb_malloc(p->mrb, (n))
#define YYFREE(o)      mrb_free(p->mrb, (o))
#define YYSTACK_USE_ALLOCA 0

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include "node.h"

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);
static void void_expr_error(parser_state *p, node *n);
static void tokadd(parser_state *p, int32_t c);

#define identchar(c) (ISALNUM(c) || (c) == '_' || !ISASCII(c))

typedef unsigned int stack_type;

#define BITSTACK_PUSH(stack, n) ((stack) = ((stack)<<1)|((n)&1))
#define BITSTACK_POP(stack)     ((stack) = (stack) >> 1)
#define BITSTACK_LEXPOP(stack)  ((stack) = ((stack) >> 1) | ((stack) & 1))
#define BITSTACK_SET_P(stack)   ((stack)&1)

#define COND_PUSH(n)    BITSTACK_PUSH(p->cond_stack, (n))
#define COND_POP()      BITSTACK_POP(p->cond_stack)
#define COND_LEXPOP()   BITSTACK_LEXPOP(p->cond_stack)
#define COND_P()        BITSTACK_SET_P(p->cond_stack)

#define CMDARG_PUSH(n)  BITSTACK_PUSH(p->cmdarg_stack, (n))
#define CMDARG_POP()    BITSTACK_POP(p->cmdarg_stack)
#define CMDARG_LEXPOP() BITSTACK_LEXPOP(p->cmdarg_stack)
#define CMDARG_P()      BITSTACK_SET_P(p->cmdarg_stack)

#define SET_LINENO(c,n) ((c)->lineno = (n))
#define NODE_LINENO(c,n) do {\
  if (n) {\
     (c)->filename_index = (n)->filename_index;\
     (c)->lineno = (n)->lineno;\
  }\
} while (0)

#define sym(x) ((mrb_sym)(intptr_t)(x))
#define nsym(x) ((node*)(intptr_t)(x))
#define nint(x) ((node*)(intptr_t)(x))
#define intn(x) ((int)(intptr_t)(x))

static inline mrb_sym
intern_cstr_gen(parser_state *p, const char *s)
{
  return mrb_intern_cstr(p->mrb, s);
}
#define intern_cstr(s) intern_cstr_gen(p,(s))

static inline mrb_sym
intern_gen(parser_state *p, const char *s, size_t len)
{
  return mrb_intern(p->mrb, s, len);
}
#define intern(s,len) intern_gen(p,(s),(len))

static inline mrb_sym
intern_gen_c(parser_state *p, const char c)
{
  return mrb_intern(p->mrb, &c, 1);
}
#define intern_c(c) intern_gen_c(p,(c))

static void
cons_free_gen(parser_state *p, node *cons)
{
  cons->cdr = p->cells;
  p->cells = cons;
}
#define cons_free(c) cons_free_gen(p, (c))

static void*
parser_palloc(parser_state *p, size_t size)
{
  void *m = mrb_pool_alloc(p->pool, size);

  if (!m) {
    MRB_THROW(p->jmp);
  }
  return m;
}

static node*
cons_gen(parser_state *p, node *car, node *cdr)
{
  node *c;

  if (p->cells) {
    c = p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (node *)parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
  c->lineno = p->lineno;
  c->filename_index = p->current_filename_index;
  return c;
}
#define cons(a,b) cons_gen(p,(a),(b))

static node*
list1_gen(parser_state *p, node *a)
{
  return cons(a, 0);
}
#define list1(a) list1_gen(p, (a))

static node*
list2_gen(parser_state *p, node *a, node *b)
{
  return cons(a, cons(b,0));
}
#define list2(a,b) list2_gen(p, (a),(b))

static node*
list3_gen(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, cons(c,0)));
}
#define list3(a,b,c) list3_gen(p, (a),(b),(c))

static node*
list4_gen(parser_state *p, node *a, node *b, node *c, node *d)
{
  return cons(a, cons(b, cons(c, cons(d, 0))));
}
#define list4(a,b,c,d) list4_gen(p, (a),(b),(c),(d))

static node*
list5_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, 0)))));
}
#define list5(a,b,c,d,e) list5_gen(p, (a),(b),(c),(d),(e))

static node*
list6_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e, node *f)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, 0))))));
}
#define list6(a,b,c,d,e,f) list6_gen(p, (a),(b),(c),(d),(e),(f))

static node*
append_gen(parser_state *p, node *a, node *b)
{
  node *c = a;

  if (!a) return b;
  while (c->cdr) {
    c = c->cdr;
  }
  if (b) {
    c->cdr = b;
  }
  return a;
}
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

static char*
parser_strndup(parser_state *p, const char *s, size_t len)
{
  char *b = (char *)parser_palloc(p, len+1);

  memcpy(b, s, len);
  b[len] = '\0';
  return b;
}
#undef strndup
#define strndup(s,len) parser_strndup(p, s, len)

static char*
parser_strdup(parser_state *p, const char *s)
{
  return parser_strndup(p, s, strlen(s));
}
#undef strdup
#define strdup(s) parser_strdup(p, s)

/* xxx ----------------------------- */

static node*
local_switch(parser_state *p)
{
  node *prev = p->locals;

  p->locals = cons(0, 0);
  return prev;
}

static void
local_resume(parser_state *p, node *prev)
{
  p->locals = prev;
}

static void
local_nest(parser_state *p)
{
  p->locals = cons(0, p->locals);
}

static void
local_unnest(parser_state *p)
{
  if (p->locals) {
    p->locals = p->locals->cdr;
  }
}

static mrb_bool
local_var_p(parser_state *p, mrb_sym sym)
{
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if (sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
  }
  return FALSE;
}

static void
local_add_f(parser_state *p, mrb_sym sym)
{
  if (p->locals) {
    p->locals->car = push(p->locals->car, nsym(sym));
  }
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

static node*
locals_node(parser_state *p)
{
  return p->locals ? p->locals->car : NULL;
}

/* (:scope (vars..) (prog...)) */
static node*
new_scope(parser_state *p, node *body)
{
  return cons((node*)NODE_SCOPE, cons(locals_node(p), body));
}

/* (:begin prog...) */
static node*
new_begin(parser_state *p, node *body)
{
  if (body) {
    return list2((node*)NODE_BEGIN, body);
  }
  return cons((node*)NODE_BEGIN, 0);
}

#define newline_node(n) (n)

/* (:rescue body rescue else) */
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  return list4((node*)NODE_RESCUE, body, resq, els);
}

static node*
new_mod_rescue(parser_state *p, node *body, node *resq)
{
  return new_rescue(p, body, list1(list3(0, 0, resq)), 0);
}

/* (:ensure body ensure) */
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ENSURE, cons(a, cons(0, b)));
}

/* (:nil) */
static node*
new_nil(parser_state *p)
{
  return list1((node*)NODE_NIL);
}

/* (:true) */
static node*
new_true(parser_state *p)
{
  return list1((node*)NODE_TRUE);
}

/* (:false) */
static node*
new_false(parser_state *p)
{
  return list1((node*)NODE_FALSE);
}

/* (:alias new old) */
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  return cons((node*)NODE_ALIAS, cons(nsym(a), nsym(b)));
}

/* (:if cond then else) */
static node*
new_if(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, b, c);
}

/* (:unless cond then else) */
static node*
new_unless(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, c, b);
}

/* (:while cond body) */
static node*
new_while(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_WHILE, cons(a, b));
}

/* (:until cond body) */
static node*
new_until(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_UNTIL, cons(a, b));
}

/* (:for var obj body) */
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  void_expr_error(p, o);
  return list4((node*)NODE_FOR, v, o, b);
}

/* (:case a ((when ...) body) ((when...) body)) */
static node*
new_case(parser_state *p, node *a, node *b)
{
  node *n = list2((node*)NODE_CASE, a);
  node *n2 = n;

  void_expr_error(p, a);
  while (n2->cdr) {
    n2 = n2->cdr;
  }
  n2->cdr = b;
  return n;
}

/* (:postexe a) */
static node*
new_postexe(parser_state *p, node *a)
{
  return cons((node*)NODE_POSTEXE, a);
}

/* (:self) */
static node*
new_self(parser_state *p)
{
  return list1((node*)NODE_SELF);
}

/* (:call a b c) */
static node*
new_call(parser_state *p, node *a, mrb_sym b, node *c, int pass)
{
  node *n = list4(nint(pass?NODE_CALL:NODE_SCALL), a, nsym(b), c);
  void_expr_error(p, a);
  NODE_LINENO(n, a);
  return n;
}

/* (:fcall self mid args) */
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  node *n = new_self(p);
  NODE_LINENO(n, c);
  n = list4((node*)NODE_FCALL, n, nsym(b), c);
  NODE_LINENO(n, c);
  return n;
}

/* (:super . c) */
static node*
new_super(parser_state *p, node *c)
{
  return cons((node*)NODE_SUPER, c);
}

/* (:zsuper) */
static node*
new_zsuper(parser_state *p)
{
  return list1((node*)NODE_ZSUPER);
}

/* (:yield . c) */
static node*
new_yield(parser_state *p, node *c)
{
  if (c) {
    if (c->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    return cons((node*)NODE_YIELD, c->car);
  }
  return cons((node*)NODE_YIELD, 0);
}

/* (:return . c) */
static node*
new_return(parser_state *p, node *c)
{
  return cons((node*)NODE_RETURN, c);
}

/* (:break . c) */
static node*
new_break(parser_state *p, node *c)
{
  return cons((node*)NODE_BREAK, c);
}

/* (:next . c) */
static node*
new_next(parser_state *p, node *c)
{
  return cons((node*)NODE_NEXT, c);
}

/* (:redo) */
static node*
new_redo(parser_state *p)
{
  return list1((node*)NODE_REDO);
}

/* (:retry) */
static node*
new_retry(parser_state *p)
{
  return list1((node*)NODE_RETRY);
}

/* (:dot2 a b) */
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT2, cons(a, b));
}

/* (:dot3 a b) */
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT3, cons(a, b));
}

/* (:colon2 b c) */
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  void_expr_error(p, b);
  return cons((node*)NODE_COLON2, cons(b, nsym(c)));
}

/* (:colon3 . c) */
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  return cons((node*)NODE_COLON3, nsym(c));
}

/* (:and a b) */
static node*
new_and(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_AND, cons(a, b));
}

/* (:or a b) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_OR, cons(a, b));
}

/* (:array a...) */
static node*
new_array(parser_state *p, node *a)
{
  return cons((node*)NODE_ARRAY, a);
}

/* (:splat . a) */
static node*
new_splat(parser_state *p, node *a)
{
  return cons((node*)NODE_SPLAT, a);
}

/* (:hash (k . v) (k . v)...) */
static node*
new_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_HASH, a);
}

/* (:sym . a) */
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_SYM, nsym(sym));
}

static mrb_sym
new_strsym(parser_state *p, node* str)
{
  const char *s = (const char*)str->cdr->car;
  size_t len = (size_t)str->cdr->cdr;

  return mrb_intern(p->mrb, s, len);
}

/* (:lvar . a) */
static node*
new_lvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_LVAR, nsym(sym));
}

/* (:gvar . a) */
static node*
new_gvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_GVAR, nsym(sym));
}

/* (:ivar . a) */
static node*
new_ivar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_IVAR, nsym(sym));
}

/* (:cvar . a) */
static node*
new_cvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CVAR, nsym(sym));
}

/* (:const . a) */
static node*
new_const(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CONST, nsym(sym));
}

/* (:undef a...) */
static node*
new_undef(parser_state *p, mrb_sym sym)
{
  return list2((node*)NODE_UNDEF, nsym(sym));
}

/* (:class class super body) */
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  void_expr_error(p, s);
  return list4((node*)NODE_CLASS, c, s, cons(locals_node(p), b));
}

/* (:sclass obj body) */
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  void_expr_error(p, o);
  return list3((node*)NODE_SCLASS, o, cons(locals_node(p), b));
}

/* (:module module body) */
static node*
new_module(parser_state *p, node *m, node *b)
{
  return list3((node*)NODE_MODULE, m, cons(locals_node(p), b));
}

/* (:def m lv (arg . body)) */
static node*
new_def(parser_state *p, mrb_sym m, node *a, node *b)
{
  return list5((node*)NODE_DEF, nsym(m), locals_node(p), a, b);
}

/* (:sdef obj m lv (arg . body)) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  void_expr_error(p, o);
  return list6((node*)NODE_SDEF, o, nsym(m), locals_node(p), a, b);
}

/* (:arg . sym) */
static node*
new_arg(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_ARG, nsym(sym));
}

/* (m o r m2 b) */
/* m: (a b c) */
/* o: ((a . e1) (b . e2)) */
/* r: a */
/* m2: (a b c) */
/* b: a */
static node*
new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, mrb_sym blk)
{
  node *n;

  n = cons(m2, nsym(blk));
  n = cons(nsym(rest), n);
  n = cons(opt, n);
  return cons(m, n);
}

/* (:block_arg . a) */
static node*
new_block_arg(parser_state *p, node *a)
{
  return cons((node*)NODE_BLOCK_ARG, a);
}

/* (:block arg body) */
static node*
new_block(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_BLOCK, locals_node(p), a, b);
}

/* (:lambda arg body) */
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_LAMBDA, locals_node(p), a, b);
}

/* (:asgn lhs rhs) */
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_ASGN, cons(a, b));
}

/* (:masgn mlhs=(pre rest post)  mrhs) */
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_MASGN, cons(a, b));
}

/* (:asgn lhs rhs) */
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  void_expr_error(p, b);
  return list4((node*)NODE_OP_ASGN, a, nsym(op), b);
}

/* (:int . i) */
static node*
new_int(parser_state *p, const char *s, int base)
{
  return list3((node*)NODE_INT, (node*)strdup(s), nint(base));
}

#ifndef MRB_WITHOUT_FLOAT
/* (:float . i) */
static node*
new_float(parser_state *p, const char *s)
{
  return cons((node*)NODE_FLOAT, (node*)strdup(s));
}
#endif

/* (:str . (s . len)) */
static node*
new_str(parser_state *p, const char *s, size_t len)
{
  return cons((node*)NODE_STR, cons((node*)strndup(s, len), nint(len)));
}

/* (:dstr . a) */
static node*
new_dstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DSTR, a);
}

/* (:str . (s . len)) */
static node*
new_xstr(parser_state *p, const char *s, int len)
{
  return cons((node*)NODE_XSTR, cons((node*)strndup(s, len), nint(len)));
}

/* (:xstr . a) */
static node*
new_dxstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DXSTR, a);
}

/* (:dsym . a) */
static node*
new_dsym(parser_state *p, node *a)
{
  return cons((node*)NODE_DSYM, a);
}

/* (:regx . (s . (opt . enc))) */
static node*
new_regx(parser_state *p, const char *p1, const char* p2, const char* p3)
{
  return cons((node*)NODE_REGX, cons((node*)p1, cons((node*)p2, (node*)p3)));
}

/* (:dregx . (a . b)) */
static node*
new_dregx(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DREGX, cons(a, b));
}

/* (:backref . n) */
static node*
new_back_ref(parser_state *p, int n)
{
  return cons((node*)NODE_BACK_REF, nint(n));
}

/* (:nthref . n) */
static node*
new_nth_ref(parser_state *p, int n)
{
  return cons((node*)NODE_NTH_REF, nint(n));
}

/* (:heredoc . a) */
static node*
new_heredoc(parser_state *p)
{
  parser_heredoc_info *inf = (parser_heredoc_info *)parser_palloc(p, sizeof(parser_heredoc_info));
  return cons((node*)NODE_HEREDOC, (node*)inf);
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

static node*
new_literal_delim(parser_state *p)
{
  return cons((node*)NODE_LITERAL_DELIM, 0);
}

/* (:words . a) */
static node*
new_words(parser_state *p, node *a)
{
  return cons((node*)NODE_WORDS, a);
}

/* (:symbols . a) */
static node*
new_symbols(parser_state *p, node *a)
{
  return cons((node*)NODE_SYMBOLS, a);
}

/* xxx ----------------------------- */

/* (:call a op) */
static node*
call_uni_op(parser_state *p, node *recv, const char *m)
{
  void_expr_error(p, recv);
  return new_call(p, recv, intern_cstr(m), 0, 1);
}

/* (:call a op b) */
static node*
call_bin_op(parser_state *p, node *recv, const char *m, node *arg1)
{
  return new_call(p, recv, intern_cstr(m), list1(list1(arg1)), 1);
}

static void
args_with_block(parser_state *p, node *a, node *b)
{
  if (b) {
    if (a->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    a->cdr = b;
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  node *n;

  switch ((enum node_type)intn(a->car)) {
  case NODE_SUPER:
  case NODE_ZSUPER:
    if (!a->cdr) a->cdr = cons(0, b);
    else {
      args_with_block(p, a->cdr, b);
    }
    break;
  case NODE_CALL:
  case NODE_FCALL:
  case NODE_SCALL:
    n = a->cdr->cdr->cdr;
    if (!n->car) n->car = cons(0, b);
    else {
      args_with_block(p, n->car, b);
    }
    break;
  default:
    break;
  }
}

static node*
negate_lit(parser_state *p, node *n)
{
  return cons((node*)NODE_NEGATE, n);
}

static node*
cond(node *n)
{
  return n;
}

static node*
ret_args(parser_state *p, node *n)
{
  if (n->cdr) {
    yyerror(p, "block argument should not be given");
    return NULL;
  }
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  if (intn(lhs->car) == NODE_LVAR) {
    local_add(p, sym(lhs->cdr));
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  node *n;

  if (intn(lhs->car) == NODE_LVAR) {
    if (!local_var_p(p, sym(lhs->cdr))) {
      n = new_fcall(p, sym(lhs->cdr), 0);
      cons_free(lhs);
      return n;
    }
  }

  return lhs;
}

typedef enum mrb_string_type  string_type;

static node*
new_strterm(parser_state *p, string_type type, int term, int paren)
{
  return cons(nint(type), cons((node*)0, cons(nint(paren), nint(term))));
}

static void
end_strterm(parser_state *p)
{
  cons_free(p->lex_strterm->cdr->cdr);
  cons_free(p->lex_strterm->cdr);
  cons_free(p->lex_strterm);
  p->lex_strterm = NULL;
}

static parser_heredoc_info *
parsing_heredoc_inf(parser_state *p)
{
  node *nd = p->parsing_heredoc;
  if (nd == NULL)
    return NULL;
  /* mrb_assert(nd->car->car == NODE_HEREDOC); */
  return (parser_heredoc_info*)nd->car->cdr;
}

static void
heredoc_treat_nextline(parser_state *p)
{
  if (p->heredocs_from_nextline == NULL)
    return;
  if (p->parsing_heredoc == NULL) {
    node *n;
    p->parsing_heredoc = p->heredocs_from_nextline;
    p->lex_strterm_before_heredoc = p->lex_strterm;
    p->lex_strterm = new_strterm(p, parsing_heredoc_inf(p)->type, 0, 0);
    n = p->all_heredocs;
    if (n) {
      while (n->cdr)
        n = n->cdr;
      n->cdr = p->parsing_heredoc;
    }
    else {
      p->all_heredocs = p->parsing_heredoc;
    }
  }
  else {
    node *n, *m;
    m = p->heredocs_from_nextline;
    while (m->cdr)
      m = m->cdr;
    n = p->all_heredocs;
    mrb_assert(n != NULL);
    if (n == p->parsing_heredoc) {
      m->cdr = n;
      p->all_heredocs = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
    else {
      while (n->cdr != p->parsing_heredoc) {
        n = n->cdr;
        mrb_assert(n != NULL);
      }
      m->cdr = n->cdr;
      n->cdr = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
  }
  p->heredocs_from_nextline = NULL;
}

static void
heredoc_end(parser_state *p)
{
  p->parsing_heredoc = p->parsing_heredoc->cdr;
  if (p->parsing_heredoc == NULL) {
    p->lstate = EXPR_BEG;
    p->cmd_start = TRUE;
    end_strterm(p);
    p->lex_strterm = p->lex_strterm_before_heredoc;
    p->lex_strterm_before_heredoc = NULL;
    p->heredoc_end_now = TRUE;
  }
  else {
    /* next heredoc */
    p->lex_strterm->car = nint(parsing_heredoc_inf(p)->type);
  }
}
#define is_strterm_type(p,str_func) (intn((p)->lex_strterm->car) & (str_func))

/* xxx ----------------------------- */


#line 1104 "parse.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    keyword_class = 258,
    keyword_module = 259,
    keyword_def = 260,
    keyword_begin = 261,
    keyword_if = 262,
    keyword_unless = 263,
    keyword_while = 264,
    keyword_until = 265,
    keyword_for = 266,
    keyword_undef = 267,
    keyword_rescue = 268,
    keyword_ensure = 269,
    keyword_end = 270,
    keyword_then = 271,
    keyword_elsif = 272,
    keyword_else = 273,
    keyword_case = 274,
    keyword_when = 275,
    keyword_break = 276,
    keyword_next = 277,
    keyword_redo = 278,
    keyword_retry = 279,
    keyword_in = 280,
    keyword_do = 281,
    keyword_do_cond = 282,
    keyword_do_block = 283,
    keyword_do_LAMBDA = 284,
    keyword_return = 285,
    keyword_yield = 286,
    keyword_super = 287,
    keyword_self = 288,
    keyword_nil = 289,
    keyword_true = 290,
    keyword_false = 291,
    keyword_and = 292,
    keyword_or = 293,
    keyword_not = 294,
    modifier_if = 295,
    modifier_unless = 296,
    modifier_while = 297,
    modifier_until = 298,
    modifier_rescue = 299,
    keyword_alias = 300,
    keyword_BEGIN = 301,
    keyword_END = 302,
    keyword__LINE__ = 303,
    keyword__FILE__ = 304,
    keyword__ENCODING__ = 305,
    tIDENTIFIER = 306,
    tFID = 307,
    tGVAR = 308,
    tIVAR = 309,
    tCONSTANT = 310,
    tCVAR = 311,
    tLABEL_TAG = 312,
    tINTEGER = 313,
    tFLOAT = 314,
    tCHAR = 315,
    tXSTRING = 316,
    tREGEXP = 317,
    tSTRING = 318,
    tSTRING_PART = 319,
    tSTRING_MID = 320,
    tNTH_REF = 321,
    tBACK_REF = 322,
    tREGEXP_END = 323,
    tUPLUS = 324,
    tUMINUS = 325,
    tPOW = 326,
    tCMP = 327,
    tEQ = 328,
    tEQQ = 329,
    tNEQ = 330,
    tGEQ = 331,
    tLEQ = 332,
    tANDOP = 333,
    tOROP = 334,
    tMATCH = 335,
    tNMATCH = 336,
    tDOT2 = 337,
    tDOT3 = 338,
    tAREF = 339,
    tASET = 340,
    tLSHFT = 341,
    tRSHFT = 342,
    tCOLON2 = 343,
    tCOLON3 = 344,
    tOP_ASGN = 345,
    tASSOC = 346,
    tLPAREN = 347,
    tLPAREN_ARG = 348,
    tRPAREN = 349,
    tLBRACK = 350,
    tLBRACE = 351,
    tLBRACE_ARG = 352,
    tSTAR = 353,
    tAMPER = 354,
    tLAMBDA = 355,
    tANDDOT = 356,
    tSYMBEG = 357,
    tREGEXP_BEG = 358,
    tWORDS_BEG = 359,
    tSYMBOLS_BEG = 360,
    tSTRING_BEG = 361,
    tXSTRING_BEG = 362,
    tSTRING_DVAR = 363,
    tLAMBEG = 364,
    tHEREDOC_BEG = 365,
    tHEREDOC_END = 366,
    tLITERAL_DELIM = 367,
    tHD_LITERAL_DELIM = 368,
    tHD_STRING_PART = 369,
    tHD_STRING_MID = 370,
    tLOWEST = 371,
    tUMINUS_NUM = 372,
    tLAST_TOKEN = 373
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 1049 "parse.y" /* yacc.c:355  */

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1268 "parse.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (parser_state *p);



/* Copy the second part of user declarations.  */

#line 1284 "parse.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   11391

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  145
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  163
/* YYNRULES -- Number of rules.  */
#define YYNRULES  559
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  987

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   373

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     144,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   131,     2,     2,     2,   129,   124,     2,
     139,   140,   127,   125,   137,   126,   143,   128,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   119,   142,
     121,   117,   120,   118,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   136,     2,   141,   123,     2,   138,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   134,   122,   135,   132,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   130,   133
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1201,  1201,  1201,  1212,  1218,  1222,  1227,  1231,  1237,
    1239,  1238,  1250,  1277,  1283,  1287,  1292,  1296,  1302,  1302,
    1306,  1310,  1314,  1318,  1322,  1326,  1330,  1335,  1336,  1340,
    1344,  1348,  1352,  1355,  1359,  1363,  1367,  1371,  1375,  1380,
    1384,  1391,  1392,  1396,  1400,  1401,  1405,  1409,  1413,  1417,
    1420,  1429,  1430,  1433,  1434,  1441,  1440,  1453,  1457,  1462,
    1466,  1471,  1475,  1480,  1484,  1488,  1492,  1496,  1502,  1506,
    1512,  1513,  1519,  1523,  1527,  1531,  1535,  1539,  1543,  1547,
    1551,  1555,  1561,  1562,  1568,  1572,  1578,  1582,  1588,  1592,
    1596,  1600,  1604,  1608,  1614,  1620,  1627,  1631,  1635,  1639,
    1643,  1647,  1653,  1659,  1666,  1670,  1673,  1677,  1681,  1688,
    1689,  1690,  1691,  1696,  1703,  1704,  1707,  1711,  1711,  1717,
    1718,  1719,  1720,  1721,  1722,  1723,  1724,  1725,  1726,  1727,
    1728,  1729,  1730,  1731,  1732,  1733,  1734,  1735,  1736,  1737,
    1738,  1739,  1740,  1741,  1742,  1743,  1744,  1745,  1748,  1748,
    1748,  1749,  1749,  1750,  1750,  1750,  1751,  1751,  1751,  1751,
    1752,  1752,  1752,  1753,  1753,  1753,  1754,  1754,  1754,  1754,
    1755,  1755,  1755,  1755,  1756,  1756,  1756,  1756,  1757,  1757,
    1757,  1757,  1758,  1758,  1758,  1758,  1759,  1759,  1762,  1766,
    1770,  1774,  1778,  1782,  1786,  1791,  1796,  1801,  1805,  1809,
    1813,  1817,  1821,  1825,  1829,  1833,  1837,  1841,  1845,  1849,
    1853,  1857,  1861,  1865,  1869,  1873,  1877,  1881,  1885,  1889,
    1893,  1897,  1901,  1905,  1909,  1913,  1917,  1921,  1925,  1929,
    1933,  1939,  1940,  1945,  1949,  1956,  1960,  1968,  1974,  1975,
    1978,  1979,  1980,  1985,  1990,  1997,  2003,  2008,  2013,  2018,
    2025,  2025,  2036,  2042,  2046,  2052,  2053,  2056,  2062,  2068,
    2073,  2080,  2085,  2090,  2097,  2098,  2099,  2100,  2101,  2102,
    2103,  2104,  2109,  2108,  2120,  2124,  2119,  2129,  2129,  2133,
    2137,  2141,  2145,  2150,  2155,  2159,  2163,  2167,  2171,  2175,
    2176,  2182,  2188,  2181,  2200,  2208,  2216,  2216,  2216,  2223,
    2223,  2223,  2230,  2236,  2241,  2243,  2240,  2252,  2250,  2266,
    2271,  2264,  2286,  2284,  2299,  2303,  2298,  2318,  2324,  2317,
    2339,  2343,  2347,  2351,  2357,  2364,  2365,  2366,  2369,  2370,
    2373,  2374,  2382,  2383,  2389,  2393,  2396,  2400,  2406,  2410,
    2416,  2420,  2424,  2428,  2432,  2436,  2440,  2444,  2448,  2454,
    2458,  2462,  2466,  2470,  2474,  2478,  2482,  2486,  2490,  2494,
    2498,  2502,  2506,  2510,  2516,  2517,  2524,  2528,  2532,  2539,
    2543,  2549,  2550,  2553,  2558,  2561,  2565,  2571,  2575,  2582,
    2581,  2594,  2604,  2608,  2613,  2620,  2624,  2628,  2632,  2636,
    2640,  2644,  2648,  2652,  2659,  2658,  2671,  2670,  2684,  2692,
    2701,  2704,  2711,  2714,  2718,  2719,  2722,  2726,  2729,  2733,
    2736,  2737,  2738,  2739,  2742,  2743,  2744,  2748,  2754,  2755,
    2761,  2766,  2765,  2776,  2780,  2786,  2790,  2796,  2800,  2806,
    2809,  2810,  2813,  2819,  2825,  2826,  2829,  2836,  2835,  2849,
    2853,  2860,  2865,  2872,  2878,  2879,  2880,  2881,  2882,  2886,
    2892,  2896,  2902,  2903,  2904,  2908,  2914,  2918,  2922,  2926,
    2930,  2936,  2942,  2946,  2950,  2954,  2958,  2962,  2970,  2977,
    2988,  2989,  2993,  2997,  2996,  3012,  3018,  3024,  3028,  3032,
    3036,  3040,  3044,  3048,  3052,  3056,  3060,  3064,  3068,  3072,
    3076,  3081,  3087,  3092,  3097,  3102,  3109,  3113,  3120,  3124,
    3130,  3134,  3140,  3147,  3154,  3161,  3165,  3171,  3175,  3181,
    3182,  3185,  3190,  3197,  3198,  3201,  3208,  3212,  3219,  3224,
    3224,  3249,  3250,  3256,  3261,  3267,  3273,  3278,  3290,  3291,
    3292,  3295,  3296,  3297,  3298,  3301,  3302,  3303,  3306,  3307,
    3310,  3314,  3320,  3321,  3327,  3328,  3331,  3332,  3335,  3338,
    3341,  3342,  3343,  3346,  3347,  3348,  3351,  3358,  3359,  3363
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "keyword_class", "keyword_module",
  "keyword_def", "keyword_begin", "keyword_if", "keyword_unless",
  "keyword_while", "keyword_until", "keyword_for", "keyword_undef",
  "keyword_rescue", "keyword_ensure", "keyword_end", "keyword_then",
  "keyword_elsif", "keyword_else", "keyword_case", "keyword_when",
  "keyword_break", "keyword_next", "keyword_redo", "keyword_retry",
  "keyword_in", "keyword_do", "keyword_do_cond", "keyword_do_block",
  "keyword_do_LAMBDA", "keyword_return", "keyword_yield", "keyword_super",
  "keyword_self", "keyword_nil", "keyword_true", "keyword_false",
  "keyword_and", "keyword_or", "keyword_not", "modifier_if",
  "modifier_unless", "modifier_while", "modifier_until", "modifier_rescue",
  "keyword_alias", "keyword_BEGIN", "keyword_END", "keyword__LINE__",
  "keyword__FILE__", "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR",
  "tIVAR", "tCONSTANT", "tCVAR", "tLABEL_TAG", "tINTEGER", "tFLOAT",
  "tCHAR", "tXSTRING", "tREGEXP", "tSTRING", "tSTRING_PART", "tSTRING_MID",
  "tNTH_REF", "tBACK_REF", "tREGEXP_END", "tUPLUS", "tUMINUS", "tPOW",
  "tCMP", "tEQ", "tEQQ", "tNEQ", "tGEQ", "tLEQ", "tANDOP", "tOROP",
  "tMATCH", "tNMATCH", "tDOT2", "tDOT3", "tAREF", "tASET", "tLSHFT",
  "tRSHFT", "tCOLON2", "tCOLON3", "tOP_ASGN", "tASSOC", "tLPAREN",
  "tLPAREN_ARG", "tRPAREN", "tLBRACK", "tLBRACE", "tLBRACE_ARG", "tSTAR",
  "tAMPER", "tLAMBDA", "tANDDOT", "tSYMBEG", "tREGEXP_BEG", "tWORDS_BEG",
  "tSYMBOLS_BEG", "tSTRING_BEG", "tXSTRING_BEG", "tSTRING_DVAR", "tLAMBEG",
  "tHEREDOC_BEG", "tHEREDOC_END", "tLITERAL_DELIM", "tHD_LITERAL_DELIM",
  "tHD_STRING_PART", "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'",
  "'>'", "'<'", "'|'", "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "tUMINUS_NUM", "'!'", "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "','",
  "'`'", "'('", "')'", "']'", "';'", "'.'", "'\\n'", "$accept", "program",
  "$@1", "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt",
  "compstmt", "stmts", "stmt", "$@3", "command_asgn", "command_rhs",
  "expr", "expr_value", "command_call", "block_command", "cmd_brace_block",
  "$@4", "command", "mlhs", "mlhs_inner", "mlhs_basic", "mlhs_item",
  "mlhs_list", "mlhs_post", "mlhs_node", "lhs", "cname", "cpath", "fname",
  "fsym", "undef_list", "$@5", "op", "reswords", "arg", "aref_args",
  "arg_rhs", "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "@6", "block_arg", "opt_block_arg", "comma", "args",
  "mrhs", "primary", "@7", "@8", "$@9", "$@10", "@11", "@12", "$@13",
  "$@14", "$@15", "$@16", "$@17", "$@18", "@19", "@20", "@21", "@22",
  "@23", "@24", "@25", "@26", "primary_value", "then", "do", "if_tail",
  "opt_else", "for_var", "f_marg", "f_marg_list", "f_margs", "block_param",
  "opt_block_param", "block_param_def", "opt_bv_decl", "bv_decls", "bvar",
  "f_larglist", "lambda_body", "do_block", "$@27", "block_call",
  "method_call", "brace_block", "@28", "@29", "case_body", "cases",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_rep", "string_interp", "@30", "xstring", "regexp", "heredoc",
  "heredoc_bodies", "heredoc_body", "heredoc_string_rep",
  "heredoc_string_interp", "@31", "words", "symbol", "basic_symbol", "sym",
  "symbols", "numeric", "variable", "var_lhs", "var_ref", "backref",
  "superclass", "$@32", "f_arglist", "f_args", "f_bad_arg", "f_norm_arg",
  "f_arg_item", "f_arg", "f_opt_asgn", "f_opt", "f_block_opt",
  "f_block_optarg", "f_optarg", "restarg_mark", "f_rest_arg",
  "blkarg_mark", "f_block_arg", "opt_f_block_arg", "singleton", "$@33",
  "assoc_list", "assocs", "assoc", "operation", "operation2", "operation3",
  "dot_or_colon", "call_op", "call_op2", "opt_terms", "opt_nl", "rparen",
  "rbracket", "trailer", "term", "nl", "terms", "none", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,    61,    63,    58,
      62,    60,   124,    94,    38,    43,    45,    42,    47,    37,
     372,    33,   126,   373,   123,   125,    91,    44,    96,    40,
      41,    93,    59,    46,    10
};
# endif

#define YYPACT_NINF -760

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-760)))

#define YYTABLE_NINF -560

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-560)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -760,   117,  2952,  -760,  7117,  9055,  9385,  5121,  -760,  8599,
    8599,  -760,  -760,  9165,  6625,  4864,  7573,  7573,  -760,  -760,
    7573,  5988,  5988,  -760,  -760,  -760,  -760,    -3,  6625,  -760,
       5,  -760,  -760,  -760,  5372,  5486,  -760,  -760,  5600,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  8713,  8713,    94,  4147,
     166,  7801,  8827,  6897,  -760,  6353,   844,   667,   944,  1001,
     458,  -760,   116,  8941,  8713,  -760,   801,  -760,  1137,  -760,
     646,  -760,  -760,   160,    88,  -760,    26,  9275,  -760,   165,
   11203,   245,   299,    66,    72,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,   198,   110,  -760,   409,    78,
    -760,  -760,  -760,  -760,  -760,   190,   190,   200,    90,   173,
    8599,   381,  4262,   642,  -760,   174,  -760,   471,  -760,  -760,
      78,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,    30,
      70,   107,   135,  -760,  -760,  -760,  -760,  -760,  -760,   140,
     191,   209,   211,  -760,   212,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,   213,
    3349,   314,   646,   251,   255,   577,   258,   284,   269,   251,
    8599,  8599,   337,  -760,  -760,   609,   373,    98,   101,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  6489,  -760,
    -760,   266,  -760,  -760,  -760,  -760,  -760,  -760,   801,  -760,
     576,  -760,   388,  -760,  -760,   801,  5258,  8713,  8713,  8713,
    -760, 11182,  -760,  -760,   277,   369,   306,  -760,  -760,  -760,
    7573,  -760,  -760,  -760,  7573,  -760,  -760,  -760,  4979,  8599,
    -760,  -760,   315,  4377,  -760,   614,   389,   424,  7231,  4147,
     312,   801,  1137,   321,   350,  -760,  7231,   321,   328,    11,
     260,  -760, 11182,   353,   260,  -760,   417,  9495,   358,   633,
     658,   710,  1446,  -760,  -760,  -760,  -760,  1063,  -760,  -760,
    -760,  -760,  -760,  -760,   811,   916,  -760,  -760,  1095,  -760,
    1098,  -760,  1138,  -760,   776,   456,   463,  -760,  -760,  -760,
    -760,  4634,  8599,  8599,  8599,  8599,  7231,  8599,  8599,  -760,
    -760,  7915,  -760,  4147,  7007,   360,  7915,  8713,  8713,  8713,
    8713,  8713,  8713,  8713,  8713,  8713,  8713,  8713,  8713,  8713,
    8713,  8713,  8713,  8713,  8713,  8713,  8713,  8713,  8713,  8713,
    8713,  8713,  8713,  2122,  -760,  7573,  -760,  9777,  -760,  -760,
   10925,  -760,  -760,  -760,  8941,  8941,  -760,   447,  -760,   646,
    -760,   730,  -760,  -760,  -760,  -760,  -760,  9859,  7573,  9941,
    3349,  8599,  -760,  -760,  -760,  -760,   532,   536,   361,  -760,
    3491,   549,  8713, 10023,  7573, 10105,  8713,  8713,  3775,   574,
     574,   108, 10187,  7573, 10269,  -760,   490,  -760,  4377,   388,
    -760,  -760,  8029,   568,  -760,  8713, 11203, 11203,  8713,   811,
    -760,  7687,  -760,  8713,  7459,  -760,   495,   321,  -760,   452,
     462,  -760,  -760,   131,   468,  -760,  -760,  6625,  3890,   467,
   10023, 10105,  8713,  1137,   321,  -760,  -760,  4749,   475,  1137,
    -760,  -760,  8143,  -760,  -760,  -760,  -760,  -760,  -760,   730,
      26,  9495,  -760,  9495, 10351,  7573, 10433,   529,  -760,  -760,
    -760,  -760,  1350,  -760,  -760,  -760,  -760,  1297,  -760,  -760,
    -760,  -760,  -760,   513,  8713,  -760,   516,   622,   538,   641,
    -760,  -760,  1149,  4377,   811,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  8713,  8713,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,   206,  8713,  -760, 11012,   277,  -760,   321,  9495,
     558,  -760,  -760,  -760,   654,   587,  1155,  -760,  -760,   731,
     394,   389,  2239,  2239,  2239,  2239,  1485,  1485,  2627,  1625,
    2239,  2239, 11262, 11262,   772,   772, 10953,  1485,  1485,   952,
     952,  1025,    58,    58,   389,   389,   389,  2425,  6102,  3073,
    6216,  -760,   190,  -760,   321,   438,  -760,   582,  -760,  -760,
    5988,  -760,  -760,  1105,   206,   206,  -760,  9713,  -760,  -760,
    -760,  -760,  -760,   801,  8599,  3349,   638,   430,  -760,   190,
     321,   190,   697,   131,  1547,  6761,  -760,  8257,   696,  -760,
     720,  -760,  5714,  5851,   321,   422,   423,   696,  -760,  -760,
    -760,  -760,    95,   147,   321,   109,   128,  8599,  6625,   589,
     718, 11203,   105,  -760, 11203, 11203,   811,  8713, 11182,  -760,
     306, 11203,  -760,  -760,   179,  7687,  7345,  -760,  -760,  -760,
     600,  -760,  -760,   243,  1137,   321,   260,   360,  -760,   205,
     430,   321,   480,   537,  -760,  -760,  1350,   126,  -760,   611,
     321,  -760,   321,    83,  1297,  -760,  -760, 11203,  1297,  -760,
    -760,   482,  -760,  -760,  -760,   616,  -760,   389,   389,  -760,
    1214,  3349,  -760,  -760, 11030,  8371,  -760,  -760,  9495,  7231,
    8941,  8713, 10515,  7573, 10597,    71,  8941,  8941,  -760,   447,
     612,   688,  8941,  8941,  -760,   447,    72,   160,  3349,  4377,
     206,  -760,   801,   741,  -760,  -760,  -760,  1297,  3349,   801,
    -760, 11012,  -760,   671,  -760,  4032,   747,  -760,  8599,   748,
    -760,  8713,  8713,   426,  8713,  8713,   749,  4519,  4519,   132,
     574,  -760,  -760,  -760,  8485,  3633, 11203,  -760,   630,  -760,
    -760,  -760,   542,   321,  1017,   636,  1363,  -760,   635,   648,
    3349,  4377,  -760,  -760,   660,   662,  -760,   665,  -760,   666,
     665,  -760,   321,   669,   677,  9605,  -760,   678,   679,  -760,
     792,  8713, 11097,  -760,  -760, 11203,  2801,  3205,   321,   435,
     461,  8713,  8713,  -760,  -760,  -760,  -760,  -760,  -760,  8941,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,   793,   689,  4377,
    3349,  -760,  -760,   321,   808,  -760,  1547,  9715,   251,  -760,
    -760,  4519,  -760,  -760,   251,  -760,  8713,  -760,   815,   820,
    -760, 11203,   142,  7345,  -760,   699,  1017,   652,  -760,  -760,
    1032,   823,   691,  1297,  -760,   482,  -760,   482,  -760,   482,
    -760,  -760,   726,  -760,  1297,  -760,   787,   751,  1297,  -760,
     482,  -760,  -760, 11115,   514, 11203, 11203,  -760,  -760,  -760,
    -760,   716,   840,  -760,  -760,  3349,   802,  -760,   774,   658,
     710,  3349,  -760,  3491,  -760,  -760,  4519,  -760,  -760,  -760,
    1017,   699,  1017,   723,  -760,   276,  -760,  -760,  -760,  -760,
     665,   728,   665,   665,  -760,   732,   733,  -760, 10679,   665,
    -760,   742,   665,  -760,  -760,   863,   730, 10761,  7573, 10843,
     536,   720,   865,   699,  1017,  1032,  -760,  -760,   482,  -760,
    -760,  -760,  1297,  -760,   482,  -760,   743,   745,  -760,   482,
    -760,  -760,  -760,   638,   430,   321,   406,   443,  -760,  -760,
    -760,   699,  -760,   665,   665,   755,   665,   665,   670,  -760,
    -760,   482,  -760,  -760,  -760,   665,  -760
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   272,     0,
       0,   296,   299,     0,     0,   544,   320,   321,   322,   323,
     284,   559,   392,   464,   463,   465,   466,   546,     0,    10,
       0,   468,   467,   469,   456,   271,   458,   457,   460,   459,
     452,   453,   414,   415,   470,   471,     0,     0,     0,     0,
     274,   559,   559,    80,   291,     0,     0,     0,     0,     0,
       0,   429,     0,     0,     0,     3,   544,     6,     9,    27,
      32,    44,    52,    51,     0,    68,     0,    72,    82,     0,
      49,   230,     0,    53,   289,   264,   265,   266,   267,   268,
     412,   411,   441,   413,   410,   462,     0,   269,   270,   250,
       5,     8,   320,   321,   284,   559,   392,     0,   104,   105,
       0,     0,     0,     0,   107,   472,   324,     0,   462,   270,
       0,   312,   158,   168,   159,   155,   184,   185,   186,   187,
     166,   181,   174,   164,   163,   179,   162,   161,   157,   182,
     156,   169,   173,   175,   167,   160,   176,   183,   178,   177,
     170,   180,   165,   154,   172,   171,   153,   151,   152,   148,
     149,   150,   109,   111,   110,   143,   144,   140,   122,   123,
     124,   131,   128,   130,   125,   126,   145,   146,   132,   133,
     137,   127,   129,   119,   120,   121,   134,   135,   136,   138,
     139,   141,   142,   147,   519,   314,   112,   113,   518,     0,
       0,     0,    50,     0,     0,     0,   462,     0,   270,     0,
       0,     0,     0,   335,   334,     0,     0,   462,   270,   177,
     170,   180,   165,   148,   149,   150,   109,   110,     0,   114,
     116,    20,   115,   432,   437,   436,   553,   556,   544,   555,
       0,   434,     0,   557,   554,   545,   456,     0,     0,     0,
     245,   257,    66,   249,   559,   265,   559,   523,    67,    65,
     559,   239,   285,    64,     0,   238,   391,    63,   546,     0,
     547,    18,     0,     0,   207,     0,   208,   281,     0,     0,
       0,   544,    15,   546,    70,    14,     0,   546,     0,   550,
     550,   231,     0,     0,   550,   521,     0,     0,    78,     0,
      88,    95,   491,   446,   445,   447,   448,     0,   444,   443,
     427,   421,   420,   423,     0,     0,   418,   439,     0,   450,
       0,   416,     0,   425,     0,   454,   455,    48,   222,   223,
       4,   545,     0,     0,     0,     0,     0,     0,     0,   379,
     381,     0,    84,     0,    76,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   541,   559,   540,     0,   543,   542,
       0,   396,   394,   290,     0,     0,   385,    57,   288,   309,
     104,   105,   106,   454,   455,   473,   307,     0,   559,     0,
       0,     0,   315,   539,   538,   317,     0,   559,   281,   326,
       0,   325,     0,     0,   559,     0,     0,     0,     0,     0,
       0,   281,     0,   559,     0,   304,     0,   117,     0,     0,
     433,   435,     0,     0,   558,     0,   258,   252,     0,   255,
     246,     0,   254,     0,   255,   247,     0,   546,   241,   559,
     559,   240,   251,   546,     0,   287,    47,     0,     0,     0,
       0,     0,     0,    17,   546,   279,    13,   545,    69,   275,
     278,   282,   552,   232,   551,   552,   234,   283,   522,    94,
      86,     0,    81,     0,     0,   559,     0,   497,   494,   493,
     492,   495,     0,   510,   514,   513,   509,   491,   292,   376,
     496,   498,   500,   559,     0,   507,   559,   512,   559,     0,
     490,   449,     0,     0,   424,   430,   428,   419,   440,   451,
     417,   426,     0,     0,     7,    21,    22,    23,    24,    25,
      45,    46,   559,     0,    28,    30,     0,    31,   546,     0,
      74,    85,    43,    33,    41,     0,   235,   188,    29,     0,
     270,   204,   212,   217,   218,   219,   214,   216,   226,   227,
     220,   221,   197,   198,   224,   225,   546,   213,   215,   209,
     210,   211,   199,   200,   201,   202,   203,   531,   536,   532,
     537,   390,   250,   388,   546,   531,   533,   532,   534,   389,
     559,   531,   532,   250,   559,   559,    34,   235,   189,    40,
     196,    55,    58,     0,     0,     0,   104,   105,   108,     0,
     546,   559,     0,   546,   491,     0,   273,   559,   559,   402,
     559,   327,   535,   280,   546,   531,   532,   559,   329,   297,
     328,   300,   535,   280,   546,   531,   532,     0,     0,     0,
       0,   257,     0,   303,   526,   525,   256,     0,   259,   253,
     559,   527,   524,   237,   255,     0,   244,   286,   548,    19,
       0,    26,   195,    71,    16,   546,   550,    87,    79,   535,
      93,   546,   531,   532,   502,   497,     0,   347,   338,   340,
     546,   336,   546,     0,     0,   483,   517,   503,     0,   486,
     511,     0,   488,   515,   442,     0,   431,   205,   206,   367,
     546,     0,   365,   364,   263,     0,    83,    77,     0,     0,
       0,     0,     0,   559,     0,     0,     0,     0,   387,    61,
       0,   393,     0,     0,   386,    59,   382,    54,     0,     0,
     559,   310,     0,     0,   393,   313,   520,   491,     0,     0,
     318,   403,   404,   559,   405,     0,   559,   332,     0,     0,
     330,     0,     0,   393,     0,     0,     0,     0,     0,   393,
       0,   118,   438,   302,     0,     0,   260,   248,   559,    11,
     276,   233,   393,   546,     0,   345,     0,   499,     0,   369,
       0,     0,   293,   501,   559,   559,   516,   559,   508,   559,
     559,   422,   546,     0,   559,     0,   505,   559,   559,   363,
       0,     0,   261,    75,    42,   236,   531,   532,   546,   531,
     532,     0,     0,    39,   193,    38,   194,    62,   549,     0,
      36,   191,    37,   192,    60,   383,   384,     0,     0,     0,
       0,   474,   308,   546,     0,   476,   491,     0,     0,   407,
     333,     0,    12,   409,     0,   294,     0,   295,     0,     0,
     305,   259,   559,   243,   337,   348,     0,   343,   339,   375,
       0,     0,     0,     0,   479,     0,   481,     0,   487,     0,
     484,   489,     0,   366,   354,   356,     0,   504,     0,   359,
       0,   361,   380,   262,   393,   229,   228,    35,   190,   397,
     395,     0,     0,   475,   316,     0,     0,   406,     0,    96,
     103,     0,   408,     0,   298,   301,     0,   399,   400,   398,
       0,   346,     0,   341,   373,   546,   371,   374,   378,   377,
     559,   559,   559,   559,   368,   559,   559,   281,     0,   559,
     506,   559,   559,    56,   311,     0,   102,     0,   559,     0,
     559,   559,     0,   344,     0,     0,   370,   480,     0,   477,
     482,   485,     0,   351,     0,   353,   535,   280,   360,     0,
     357,   362,   319,    99,   101,   546,   531,   532,   401,   331,
     306,   342,   372,   559,   559,   559,   559,   559,    97,   478,
     352,     0,   349,   355,   358,   559,   350
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -760,  -760,  -760,   444,  -760,    36,  -760,  -235,    64,  -760,
      27,  -760,  -214,  -339,  1176,    -8,   -54,  -760,  -511,  -760,
       4,   894,  -149,    -7,   -66,  -258,  -432,   -27,  1691,   -59,
     905,    25,     7,  -760,  -760,    29,  -760,  1119,  -760,   338,
      46,  -447,  -267,    76,   -14,  -760,  -344,  -239,  -179,    44,
    -313,    17,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,  -760,
    -760,  -760,   617,  -203,  -410,   -28,  -549,  -760,  -714,  -673,
     252,  -760,  -489,  -760,  -591,  -760,   -13,  -760,  -760,   208,
    -760,  -760,  -760,   -83,  -760,  -760,  -411,  -760,     6,  -760,
    -760,  -760,  -760,    -4,    14,  -180,  -760,  -760,  -760,  -760,
     623,  -255,  -760,   700,  -760,  -760,  -760,     0,  -760,  -760,
    -760,  1386,  1890,   937,  1251,  -760,  -760,   111,  -275,  -738,
     157,  -631,   156,  -634,  -597,  -759,    85,   278,  -760,  -601,
    -760,  -246,    74,  -760,  -760,  -760,    38,  -429,  2325,  -336,
    -760,  -760,   -78,  -760,    -1,   -24,  -249,  -510,  -208,    10,
     144,    15,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    65,    66,    67,   272,   406,   407,   281,
     282,   457,    69,   543,    70,   203,    71,    72,   602,   730,
      73,    74,   283,    75,    76,    77,   482,    78,   204,   114,
     115,   229,   230,   231,   638,   580,   197,    80,   288,   547,
     581,   262,   447,   448,   263,   264,   253,   440,   446,   449,
     537,    81,   200,   286,   665,   287,   302,   683,   210,   757,
     211,   758,   637,   906,   605,   603,   830,   400,   402,   614,
     615,   836,   275,   410,   629,   749,   750,   216,   678,   679,
     680,   792,   701,   702,   778,   915,   916,   498,   782,   340,
     532,    83,    84,   388,   595,   594,   433,   909,   618,   743,
     838,   842,    85,    86,   315,   316,   513,    87,    88,    89,
     646,   239,   240,   241,   428,    90,    91,    92,   309,    93,
      94,   206,   207,    97,   208,   396,   604,   738,   739,   500,
     501,   502,   503,   504,   505,   796,   797,   506,   507,   508,
     509,   786,   685,   199,   401,   293,   450,   257,    99,   609,
     583,   405,   399,   380,   242,   454,   455,   721,   473,   411,
     270,   245,   285
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     100,   383,   209,   269,   377,   379,   418,   238,   267,   327,
     631,   345,   255,   255,   232,   652,   255,   445,   640,   265,
     250,   250,   116,   116,   250,   243,   298,   499,   232,    68,
     116,    68,   195,   548,   468,   271,   196,   582,   470,   481,
     101,   590,   284,   196,   593,   596,   599,   255,   255,   291,
     295,   668,   392,   783,   256,   256,   510,   196,   256,   515,
     254,   254,   858,   611,   254,   330,   795,   261,   266,   746,
     116,   318,   320,   322,   324,   441,   243,   582,   756,   590,
     308,   331,   476,   785,   196,   387,   478,   789,   611,   290,
     294,   788,   252,   258,   116,   289,   259,   649,   381,   798,
     734,   855,   649,   265,   381,   728,   729,   707,   584,   793,
     472,   475,   780,   280,   753,   475,  -528,     3,  -464,   930,
     -99,   409,   917,   -96,   759,   611,  -103,   415,   811,   347,
     464,   610,   542,  -102,   -98,   517,   268,   424,   517,   273,
     517,   237,   517,   724,   517,   386,   726,   624,   439,   277,
     611,   261,   266,  -100,   378,   237,   634,   -97,  -463,   244,
     745,   772,   432,   342,   724,   612,   386,   374,   337,   338,
     542,   542,  -101,  -464,   325,   326,   280,   675,  -456,   488,
     489,   490,   491,   911,   515,   370,   371,   372,   339,   515,
     812,  -456,   781,   930,   538,  -465,   858,   461,   653,  -529,
     384,   872,   419,   420,   657,   341,   382,   917,   817,   376,
     244,   445,   382,  -463,   824,   663,   233,   260,   671,   234,
     235,   486,   682,  -466,  -528,   481,  -456,   652,  -468,  -528,
     -91,   480,   -91,  -456,  -531,   -88,   783,   429,   -95,   943,
     795,   829,   439,   783,   795,   -94,   -90,   236,   243,   237,
    -465,   510,   442,   308,   442,   434,   255,   196,   451,   696,
     255,  -460,   921,   774,   250,   -92,   788,   409,   250,   -89,
     655,   971,   284,   926,  -460,   237,   803,   931,  -466,  -467,
     466,   481,   346,  -468,   -93,   699,  -532,   534,  -461,   706,
     233,   243,   544,   234,   235,   395,   467,  -469,   884,  -456,
    -460,   403,   256,   907,   742,   463,  -277,  -529,   254,  -460,
    -277,   649,  -529,   469,   116,   -96,  -460,   540,   795,  -242,
    -242,   512,   -91,  -242,   525,   526,   527,   528,   700,   260,
     544,   544,   517,  -324,  -467,   -88,   284,   459,   608,   268,
     452,   434,   -91,   280,  -531,   -91,  -324,   244,  -461,   -91,
     850,   975,  -469,   244,  -456,  -460,   404,   705,    68,   417,
     -69,   116,   233,   529,   736,   234,   235,   524,   510,   408,
     733,   255,   412,   451,   416,   -96,   582,   813,   590,   250,
     -83,  -324,   244,   820,   822,   536,  -103,   373,  -324,   244,
     536,   696,   421,   236,   255,   237,   451,   444,   425,   515,
     374,   515,   250,   427,   237,   619,   588,   280,   432,   588,
     255,   767,   451,   945,   439,   667,   770,   480,   250,   255,
     237,   451,   -98,   589,   652,   244,   443,   250,   588,   630,
     630,   777,   390,   474,   474,   375,   391,   255,   474,   765,
     255,   908,   376,   444,   588,   589,   808,   442,   442,   458,
     481,   462,   465,   588,   799,   978,   100,   232,   771,  -100,
     347,   589,   833,   764,   659,   237,   800,   -68,   255,   471,
     589,   714,   479,   480,   620,   244,   642,   434,  -102,   650,
     887,   255,   627,   451,   385,    68,   196,   475,   477,   250,
     588,   510,   639,   827,   664,   483,   542,   541,   116,   385,
     116,   686,   542,   834,   686,   -90,   686,   589,   542,   542,
     666,  -103,   754,   755,   462,   588,   846,   -98,  -280,   323,
     -98,   -98,   311,   312,   854,   722,  -103,   522,   722,   767,
     703,  -280,   589,   675,   523,   488,   489,   490,   491,   -98,
    -100,  -102,   715,   -97,   601,   861,   -95,   616,   -98,   617,
     -98,   723,   -98,   511,  -100,   -98,   116,  -100,  -100,   397,
     720,   -94,   -92,   244,   244,   621,  -280,   -89,   719,  -532,
     313,   314,   374,  -280,   492,   -90,   725,   695,  -100,   727,
     689,   494,   692,   643,   893,  -100,   720,  -100,   265,   654,
     510,   265,   703,   703,   248,   892,   732,   -90,   515,   656,
     720,   628,   661,   611,   819,   542,   495,   398,   658,   265,
     720,   244,   -83,   731,   376,   744,   747,   -90,   747,    82,
     -90,    82,   117,   117,   -90,   747,   205,   205,   718,   760,
     215,   -97,   205,   205,   205,   901,   261,   205,   232,   261,
     740,   903,   480,   825,   196,   761,   674,   720,   442,   681,
     684,   255,   255,   688,   -92,   718,   544,   261,   779,   -89,
     935,   536,   544,   815,  -535,   413,    82,   196,   544,   544,
     299,   965,   723,   690,   -92,   691,   779,   -92,   374,   -89,
     205,   -92,   -89,   337,   338,   233,   -89,   430,   234,   235,
     234,   235,   693,   768,   299,   708,  -393,   422,   709,  -100,
     393,   394,   460,   675,   710,   488,   489,   490,   491,   255,
     374,   451,   735,   414,   745,   374,   236,   250,   237,   -92,
     376,   484,   598,   600,   762,   116,  -535,   205,   703,    82,
     317,   311,   312,   763,   374,   769,   804,   748,   745,  -535,
     844,   839,   831,   588,   843,   423,  -462,   244,   776,   835,
     414,   791,   376,   818,   598,   600,   832,   376,  -393,  -462,
     589,   841,   837,   845,   847,   544,   442,   853,   779,   485,
     630,  -393,  -535,   856,  -535,   859,   376,  -531,   819,   313,
     314,  -535,   686,   686,   720,   686,   244,   686,   686,   912,
     860,   873,   686,   828,  -462,   686,   686,   863,  -270,   865,
     662,  -462,   867,   869,  -393,   -97,  -393,   882,   889,   840,
     474,  -270,   116,  -393,   874,   878,   880,    82,  -281,   712,
     939,   848,   849,   894,   890,   -89,   919,   205,   205,   852,
     904,  -281,   374,   681,   775,   905,   910,   521,   918,   928,
     311,   312,   927,   347,   787,   862,  -270,   790,   924,   255,
     747,   933,   374,  -270,   116,   934,   794,   936,   864,   866,
     944,   868,   937,   870,   871,   948,  -281,   713,   875,   952,
     954,   879,   881,  -281,   376,   374,   244,   205,   962,   959,
     970,   205,  -531,   244,  -532,   205,   205,   398,   313,   314,
      82,   946,   981,   891,   376,    82,    82,   368,   369,   370,
     371,   372,   660,    82,   244,   902,   310,   213,   311,   312,
     938,   121,   233,   969,   299,   234,   235,   376,   686,   686,
     686,   686,   233,   686,   686,   234,   235,   686,   773,   686,
     686,   681,   972,   681,   255,   826,   451,   514,   619,   747,
     431,   720,   250,   236,   198,   237,   968,   895,    82,   205,
     205,   205,   205,    82,   205,   205,   313,   314,   205,   925,
      82,   299,   784,   549,     0,   940,     0,   941,   588,     0,
     942,   686,   686,   686,   686,   686,     0,     0,   516,     0,
     311,   312,   244,   686,     0,   589,     0,     0,   244,     0,
       0,     0,   205,     0,   947,   949,   950,   951,     0,   953,
     955,   549,   549,   958,     0,   960,   961,   319,   311,   312,
       0,     0,     0,   681,   913,   205,     0,    82,   205,   920,
       0,   922,     0,   347,     0,   923,     0,    82,   313,   314,
       0,   205,     0,     0,   929,    82,   932,     0,   360,   361,
     205,     0,     0,     0,     0,    82,     0,   979,   980,   982,
     983,   984,     0,     0,   814,   816,   313,   314,     0,   986,
     821,   823,     0,     0,   321,   311,   312,   681,   675,   681,
     488,   489,   490,   491,     0,    82,   367,   368,   369,   370,
     371,   372,     0,   914,    82,   488,   489,   490,   491,   814,
     816,     0,   821,   823,     0,     0,   347,     0,   299,     0,
     299,   681,   205,     0,   973,  -559,     0,     0,   974,   676,
     976,   360,   361,   313,   314,   977,     0,     0,  -559,  -559,
    -559,  -559,  -559,  -559,     0,  -559,   511,   311,   312,     0,
      82,  -559,  -559,     0,     0,   251,   251,   985,     0,   251,
       0,     0,  -559,  -559,     0,  -559,  -559,  -559,  -559,  -559,
     368,   369,   370,   371,   372,     0,   299,   888,   518,   311,
     312,   519,   311,   312,     0,   274,   276,     0,     0,     0,
     251,   292,     0,     0,     0,   313,   314,   332,   333,   334,
     335,   336,   328,   329,   888,   202,   202,     0,     0,     0,
       0,   202,     0,  -559,     0,     0,     0,     0,     0,   711,
       0,   520,   311,   312,     0,     0,  -559,   313,   314,     0,
     313,   314,   694,   311,   312,     0,  -559,     0,     0,  -559,
    -559,   205,    82,     0,     0,     0,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,  -559,
    -559,   360,   361,     0,   260,  -559,     0,  -559,  -559,  -559,
     313,   314,     0,    98,   205,    98,   119,   119,     0,     0,
       0,   313,   314,     0,   218,   487,     0,   488,   489,   490,
     491,     0,     0,   362,     0,   363,   364,   365,   366,   367,
     368,   369,   370,   371,   372,     0,   389,     0,     0,     0,
       0,     0,  -257,     0,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,   301,     0,   492,     0,     0,     0,
       0,     0,   493,   494,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,     0,   299,    82,   549,   301,     0,
     205,     0,     0,   549,   205,     0,     0,     0,   495,   549,
     549,   496,     0,     0,     0,    82,    82,     0,   487,     0,
     488,   489,   490,   491,     0,    82,     0,     0,   237,     0,
       0,     0,    82,    98,     0,   205,   436,   437,   328,     0,
       0,     0,     0,     0,    82,    82,     0,     0,     0,   251,
       0,     0,    82,   251,     0,     0,   202,   202,    95,   492,
      95,   118,   118,   118,     0,   493,   494,    82,    82,   217,
       0,   675,     0,   488,   489,   490,   491,     0,     0,     0,
       0,     0,   877,     0,   675,     0,   488,   489,   490,   491,
       0,   495,     0,     0,   496,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    95,   549,     0,     0,   300,
       0,     0,   676,     0,   453,   456,    82,    82,   677,     0,
       0,    98,     0,     0,   898,   676,     0,     0,    82,     0,
     535,   857,     0,   300,     0,   546,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,     0,     0,   251,     0,     0,   487,    95,   488,
     489,   490,   491,   597,   597,     0,     0,     0,   202,   202,
     202,   202,    82,   530,   531,     0,     0,   251,    82,     0,
      82,     0,     0,    82,    98,     0,     0,     0,     0,    98,
      98,   597,     0,   251,     0,   597,   597,    98,   492,     0,
       0,     0,   251,     0,   493,   494,     0,     0,   301,     0,
       0,   641,     0,     0,   644,   205,   347,   645,     0,     0,
     648,     0,   651,   292,     0,     0,     0,     0,     0,     0,
     495,   360,   361,   496,     0,     0,     0,   613,     0,     0,
       0,   597,    98,     0,     0,   497,    95,    98,     0,     0,
       0,   648,     0,     0,    98,   301,     0,   550,   487,     0,
     488,   489,   490,   491,   251,     0,     0,   365,   366,   367,
     368,   369,   370,   371,   372,     0,     0,     0,     0,     0,
       0,     0,     0,   687,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   550,   550,     0,     0,   492,
       0,   697,   698,     0,     0,   493,   494,     0,     0,     0,
       0,    98,   704,     0,     0,     0,     0,     0,     0,    95,
       0,    98,     0,     0,    95,    95,     0,     0,     0,    98,
       0,   495,    95,     0,   496,     0,     0,     0,     0,    98,
       0,     0,     0,   300,     0,     0,   737,     0,     0,     0,
       0,     0,     0,    79,     0,    79,   347,   348,   349,   350,
     351,   352,   353,   354,   214,   356,   357,     0,     0,    98,
       0,   360,   361,     0,     0,     0,     0,    95,    98,     0,
       0,     0,    95,     0,     0,     0,     0,     0,     0,    95,
     300,     0,   301,     0,   301,     0,   741,     0,     0,     0,
      79,     0,     0,     0,     0,   363,   364,   365,   366,   367,
     368,   369,   370,   371,   372,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    98,     0,   766,     0,     0,     0,
       0,     0,     0,     0,   648,   292,     0,     0,     0,     0,
     202,     0,     0,     0,     0,     0,    95,     0,     0,     0,
     301,     0,     0,     0,     0,     0,    95,     0,     0,     0,
       0,     0,     0,    79,    95,     0,     0,     0,     0,     0,
       0,     0,     0,   202,    95,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   802,     0,     0,     0,     0,   597,
     805,     0,   251,     0,     0,   597,   597,     0,     0,     0,
       0,   597,   597,     0,    95,     0,     0,     0,     0,     0,
       0,     0,     0,    95,     0,     0,    98,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   300,     0,   300,
     597,   597,     0,   597,   597,     0,     0,     0,     0,     0,
       0,     0,     0,   851,     0,     0,     0,     0,     0,     0,
       0,    79,    96,     0,    96,     0,     0,     0,     0,    95,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     883,     0,     0,     0,   202,   300,     0,     0,     0,     0,
     885,   886,     0,     0,     0,     0,     0,     0,   597,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,     0,     0,     0,   301,
      98,   550,     0,     0,    79,   597,     0,   550,     0,    79,
      79,     0,   292,   550,   550,     0,     0,    79,     0,    98,
      98,     0,     0,     0,     0,     0,     0,     0,     0,    98,
       0,    95,     0,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    96,     0,     0,     0,     0,     0,    98,    98,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    79,     0,     0,     0,     0,    79,     0,     0,
       0,    98,    98,     0,    79,     0,     0,   545,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   251,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     550,     0,     0,     0,     0,   545,   545,     0,     0,     0,
      98,    98,     0,     0,     0,     0,     0,    95,   900,     0,
      96,    79,    98,     0,   300,    95,     0,     0,     0,     0,
       0,    79,     0,     0,     0,     0,     0,     0,     0,    79,
       0,     0,     0,     0,    95,    95,     0,     0,     0,    79,
       0,     0,     0,     0,    95,     0,     0,     0,     0,     0,
       0,    95,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    95,    95,     0,    98,     0,     0,    79,
       0,    95,    98,     0,    98,     0,     0,    98,    79,     0,
       0,     0,     0,    96,     0,     0,    95,    95,    96,    96,
       0,     0,     0,   577,   578,     0,    96,   579,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,   174,   175,    79,     0,   176,   177,   178,   179,
       0,     0,     0,     0,     0,    95,    95,     0,     0,     0,
     180,    96,     0,   899,     0,     0,    96,    95,     0,     0,
       0,     0,     0,    96,     0,     0,    96,     0,     0,     0,
       0,     0,   181,   182,   183,   184,   185,   186,   187,   188,
     189,   190,     0,   191,   192,     0,     0,     0,     0,     0,
     193,   260,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    96,    96,     0,     0,     0,     0,
       0,    95,     0,     0,     0,     0,     0,    95,     0,    95,
      96,     0,    95,     0,     0,     0,    79,     0,     0,     0,
      96,     0,     0,     0,     0,     0,     0,     0,    96,     0,
     347,  -560,  -560,  -560,  -560,   352,   353,     0,    96,  -560,
    -560,     0,     0,     0,     0,   360,   361,     0,     0,     0,
     120,   120,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,     0,     0,     0,     0,     0,    96,     0,   363,
     364,   365,   366,   367,   368,   369,   370,   371,   372,     0,
       0,   120,   120,     0,     0,     0,   120,   120,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,    79,     0,     0,     0,     0,     0,     0,     0,
      79,   545,   120,    96,     0,     0,     0,   545,     0,     0,
       0,     0,     0,   545,   545,     0,     0,     0,     0,    79,
      79,     0,     0,     0,     0,  -535,     0,     0,     0,    79,
       0,     0,     0,     0,     0,     0,    79,     0,  -535,  -535,
    -535,     0,  -535,  -535,     0,  -535,     0,     0,    79,    79,
       0,  -535,     0,     0,     0,     0,    79,     0,     0,     0,
       0,     0,  -535,  -535,     0,  -535,  -535,  -535,  -535,  -535,
       0,    79,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    96,  -535,  -535,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,     0,
     545,  -535,  -535,  -535,     0,   716,     0,     0,     0,     0,
      79,    79,     0,     0,     0,     0,  -535,     0,   897,     0,
       0,     0,    79,     0,     0,     0,  -535,     0,     0,  -535,
    -535,     0,   -99,  -535,     0,  -535,  -535,  -535,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,     0,     0,     0,     0,  -535,
    -535,  -535,   -91,     0,     0,  -535,     0,  -535,  -535,  -535,
       0,     0,   120,   120,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    79,     0,     0,     0,
       0,    96,    79,     0,    79,     0,     0,    79,     0,    96,
      96,     0,     0,     0,     0,     0,    96,     0,     0,     0,
       0,     0,    96,    96,     0,     0,     0,     0,    96,    96,
       0,     0,   120,     0,     0,     0,     0,     0,    96,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    96,    96,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
      96,    96,   120,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   347,   348,
     349,   350,   351,   352,   353,     0,     0,   356,   357,    96,
       0,     0,     0,   360,   361,     0,     0,     0,     0,    96,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,     0,     0,   120,     0,     0,
       0,   120,   120,     0,     0,     0,     0,   363,   364,   365,
     366,   367,   368,   369,   370,   371,   372,   120,     0,     0,
     120,     0,     0,   120,     0,     0,   120,     0,   120,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    96,     0,   120,     0,     0,
       0,    96,     0,    96,     0,     0,    96,   120,     0,     0,
       0,  -535,     0,     0,     0,     0,   120,     0,   120,     0,
       0,     0,     0,     0,  -535,  -535,  -535,     0,  -535,  -535,
       0,  -535,     0,     0,     0,     0,     0,  -535,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,  -535,  -535,
       0,  -535,  -535,  -535,  -535,  -535,     0,   120,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,     0,     0,  -535,  -535,  -535,
       0,   716,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -535,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -535,     0,     0,  -535,  -535,     0,   -99,  -535,
       0,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,
    -535,     0,     0,     0,     0,  -535,  -535,  -535,  -535,     0,
       0,  -535,   120,  -535,  -535,  -535,     0,     0,     0,     0,
       0,     0,  -559,     4,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,     0,     0,     0,
       0,    15,   120,    16,    17,    18,    19,     0,     0,     0,
     120,   120,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,   120,     0,     0,   120,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,  -559,     0,     0,  -559,  -559,     0,     0,
       0,     0,     0,  -280,     0,     0,   120,   120,     0,   120,
     120,     0,    62,    63,    64,     0,  -280,  -280,  -280,   120,
    -280,  -280,     0,  -280,  -559,     0,  -559,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -280,  -280,     0,  -280,  -280,  -280,  -280,  -280,     0,     0,
     120,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,   120,     0,     0,
       0,     0,     0,     0,  -280,  -280,  -280,  -280,  -280,  -280,
    -280,  -280,  -280,  -280,  -280,  -280,  -280,     0,     0,  -280,
    -280,  -280,   120,   717,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,  -280,     0,     0,     0,   120,     0,
       0,     0,     0,     0,  -280,     0,     0,  -280,  -280,     0,
    -101,  -280,     0,  -280,  -280,  -280,  -280,  -280,  -280,  -280,
    -280,  -280,  -280,     0,     0,  -280,     0,     0,  -280,  -280,
     -93,     0,     0,  -280,     0,  -280,  -280,  -280,  -280,  -280,
    -280,     0,  -280,  -280,     0,  -280,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -280,  -280,     0,  -280,  -280,  -280,  -280,  -280,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -280,  -280,  -280,  -280,
    -280,  -280,  -280,  -280,  -280,  -280,  -280,  -280,  -280,     0,
       0,  -280,  -280,  -280,     0,   717,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -280,     0,     0,  -280,
    -280,     0,  -101,  -280,     0,  -280,  -280,  -280,  -280,  -280,
    -280,  -280,  -280,  -280,  -280,     0,     0,     0,     0,     0,
    -280,  -280,  -280,     0,     0,  -280,     0,  -280,  -280,  -280,
     278,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,  -559,  -559,  -559,     0,     0,  -559,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,     0,
       0,    49,    50,     0,    51,    52,     0,    53,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
    -559,     0,     0,  -559,  -559,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -559,   278,  -559,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,  -559,     0,  -559,  -559,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,     0,     0,    49,    50,     0,    51,    52,     0,    53,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,  -559,     0,     0,  -559,  -559,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -559,   278,  -559,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,  -559,     0,
       0,  -559,    15,  -559,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,     0,     0,    49,    50,     0,    51,    52,
       0,    53,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,  -559,     0,     0,  -559,  -559,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -559,   278,  -559,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -559,     0,     0,  -559,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,     0,     0,    49,    50,     0,
      51,    52,     0,    53,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,  -559,     0,     0,  -559,
    -559,     4,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    62,    63,    64,     0,    15,
       0,    16,    17,    18,    19,     0,     0,  -559,     0,  -559,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
       0,     0,    49,    50,     0,    51,    52,     0,    53,     0,
      54,     0,    55,    56,    57,    58,    59,    60,     0,     0,
      61,  -559,     0,     0,  -559,  -559,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,    64,     0,     0,  -559,     0,     0,     0,     0,
       0,     0,  -559,   278,  -559,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,  -559,  -559,     0,     0,
       0,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,  -559,     0,     0,  -559,  -559,   278,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    62,    63,    64,     0,    15,     0,    16,    17,
      18,    19,     0,     0,  -559,     0,  -559,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,     0,     0,   279,
      50,     0,    51,    52,     0,    53,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,  -559,     0,
       0,  -559,  -559,   278,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    62,    63,    64,
       0,    15,     0,    16,    17,    18,    19,  -559,     0,  -559,
       0,  -559,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,  -559,     0,     0,  -559,  -559,   278,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    62,    63,    64,     0,    15,     0,    16,    17,
      18,    19,  -559,     0,  -559,     0,  -559,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,     0,     0,    49,
      50,     0,    51,    52,     0,    53,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,  -559,     0,
       0,  -559,  -559,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    62,    63,    64,
       0,     0,  -559,     0,     0,     0,     0,     0,     0,  -559,
     278,  -559,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,  -559,     0,     0,     0,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,     0,
       0,    49,    50,     0,    51,    52,     0,    53,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
    -559,     0,     0,  -559,  -559,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    62,
      63,    64,     0,    15,     0,    16,    17,    18,    19,     0,
       0,  -559,     0,  -559,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,     0,     0,    49,    50,     0,    51,
      52,     0,    53,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,   233,     0,     0,   234,   235,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    62,    63,    64,     0,    15,     0,
      16,    17,    18,    19,     0,     0,   236,     0,   237,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,     0,
       0,    49,    50,     0,    51,    52,     0,    53,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
     233,     0,     0,   234,   235,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,    62,
      63,    64,     0,    15,     0,    16,    17,    18,    19,     0,
       0,   236,     0,   237,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,   233,     0,     0,   234,   235,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,    62,    63,    64,     0,    15,     0,
      16,    17,    18,    19,     0,     0,   236,     0,   237,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   237,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,     0,     0,
       0,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,     0,     0,     0,     0,     0,   156,   157,   158,   159,
     160,   161,   162,   163,    36,    37,   164,    39,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,     0,     0,     0,     0,     0,   193,
     194,  -528,  -528,  -528,  -528,  -528,  -528,  -528,  -528,  -528,
       0,     0,     0,     0,     0,     0,     0,  -528,     0,  -528,
    -528,  -528,  -528,     0,  -528,     0,     0,     0,  -528,  -528,
    -528,  -528,  -528,  -528,  -528,     0,     0,  -528,     0,     0,
       0,     0,     0,     0,     0,     0,  -528,  -528,  -528,  -528,
    -528,  -528,  -528,  -528,  -528,   435,  -528,  -528,  -528,     0,
       0,  -528,     0,     0,  -528,  -528,     0,  -528,  -528,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -528,     0,     0,
    -528,  -528,     0,  -528,  -528,     0,  -528,  -528,  -528,     0,
    -528,  -528,  -528,  -528,  -528,  -528,     0,     0,  -528,     0,
       0,     0,     0,     0,     0,  -528,  -528,  -528,  -528,  -528,
    -528,  -528,  -528,  -528,     0,     0,     0,     0,  -528,  -528,
    -528,  -528,  -528,  -528,  -528,  -528,  -528,  -528,  -528,     0,
       0,     0,  -528,  -528,  -528,  -528,  -528,  -528,  -528,     0,
       0,  -528,     0,     0,     0,     0,     0,     0,     0,     0,
    -528,  -528,  -528,  -528,  -528,  -528,  -528,  -528,  -528,     0,
    -528,  -528,  -528,     0,     0,  -528,     0,     0,  -528,  -528,
       0,  -528,  -528,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -528,     0,     0,  -528,  -528,     0,  -528,  -528,     0,
    -528,  -528,  -528,     0,  -528,  -528,  -528,  -528,  -528,  -528,
       0,     0,  -528,     0,     0,     0,     0,     0,     0,  -530,
    -530,  -530,  -530,  -530,  -530,  -530,  -530,  -530,     0,     0,
       0,     0,  -528,  -528,  -528,  -530,  -528,  -530,  -530,  -530,
    -530,  -528,  -530,     0,     0,     0,  -530,  -530,  -530,  -530,
    -530,  -530,  -530,     0,     0,  -530,     0,     0,     0,     0,
       0,     0,     0,     0,  -530,  -530,  -530,  -530,  -530,  -530,
    -530,  -530,  -530,     0,  -530,  -530,  -530,     0,     0,  -530,
       0,     0,  -530,  -530,     0,  -530,  -530,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -530,     0,     0,  -530,  -530,
       0,  -530,  -530,     0,  -530,  -530,  -530,     0,  -530,  -530,
    -530,  -530,  -530,  -530,     0,     0,  -530,     0,     0,     0,
       0,     0,     0,  -529,  -529,  -529,  -529,  -529,  -529,  -529,
    -529,  -529,     0,     0,     0,     0,  -530,  -530,  -530,  -529,
    -530,  -529,  -529,  -529,  -529,  -530,  -529,     0,     0,     0,
    -529,  -529,  -529,  -529,  -529,  -529,  -529,     0,     0,  -529,
       0,     0,     0,     0,     0,     0,     0,     0,  -529,  -529,
    -529,  -529,  -529,  -529,  -529,  -529,  -529,     0,  -529,  -529,
    -529,     0,     0,  -529,     0,     0,  -529,  -529,     0,  -529,
    -529,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -529,
       0,     0,  -529,  -529,     0,  -529,  -529,     0,  -529,  -529,
    -529,     0,  -529,  -529,  -529,  -529,  -529,  -529,     0,     0,
    -529,     0,     0,     0,     0,     0,     0,  -531,  -531,  -531,
    -531,  -531,  -531,  -531,  -531,  -531,     0,     0,     0,     0,
    -529,  -529,  -529,  -531,  -529,  -531,  -531,  -531,  -531,  -529,
       0,     0,     0,     0,  -531,  -531,  -531,  -531,  -531,  -531,
    -531,     0,     0,  -531,     0,     0,     0,     0,     0,     0,
       0,     0,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,
    -531,     0,  -531,  -531,  -531,     0,     0,  -531,     0,     0,
    -531,  -531,     0,  -531,  -531,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -531,   751,     0,  -531,  -531,     0,  -531,
    -531,     0,  -531,  -531,  -531,     0,  -531,  -531,  -531,  -531,
    -531,  -531,     0,     0,  -531,     0,     0,     0,     0,     0,
       0,   -99,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -531,  -531,  -531,     0,     0,     0,
       0,     0,     0,  -531,  -532,  -532,  -532,  -532,  -532,  -532,
    -532,  -532,  -532,     0,     0,     0,     0,     0,     0,     0,
    -532,     0,  -532,  -532,  -532,  -532,     0,     0,     0,     0,
       0,  -532,  -532,  -532,  -532,  -532,  -532,  -532,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,     0,     0,  -532,
    -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,     0,  -532,
    -532,  -532,     0,     0,  -532,     0,     0,  -532,  -532,     0,
    -532,  -532,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,   752,     0,  -532,  -532,     0,  -532,  -532,     0,  -532,
    -532,  -532,     0,  -532,  -532,  -532,  -532,  -532,  -532,     0,
       0,  -532,     0,     0,     0,     0,     0,     0,  -101,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,  -532,  -532,     0,     0,     0,     0,     0,     0,
    -532,  -250,  -250,  -250,  -250,  -250,  -250,  -250,  -250,  -250,
       0,     0,     0,     0,     0,     0,     0,  -250,     0,  -250,
    -250,  -250,  -250,     0,     0,     0,     0,     0,  -250,  -250,
    -250,  -250,  -250,  -250,  -250,     0,     0,  -250,     0,     0,
       0,     0,     0,     0,     0,     0,  -250,  -250,  -250,  -250,
    -250,  -250,  -250,  -250,  -250,     0,  -250,  -250,  -250,     0,
       0,  -250,     0,     0,  -250,  -250,     0,  -250,  -250,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -250,     0,     0,
    -250,  -250,     0,  -250,  -250,     0,  -250,  -250,  -250,     0,
    -250,  -250,  -250,  -250,  -250,  -250,     0,     0,  -250,     0,
       0,     0,     0,     0,     0,  -533,  -533,  -533,  -533,  -533,
    -533,  -533,  -533,  -533,     0,     0,     0,     0,  -250,  -250,
    -250,  -533,     0,  -533,  -533,  -533,  -533,   260,     0,     0,
       0,     0,  -533,  -533,  -533,  -533,  -533,  -533,  -533,     0,
       0,  -533,     0,     0,     0,     0,     0,     0,     0,     0,
    -533,  -533,  -533,  -533,  -533,  -533,  -533,  -533,  -533,     0,
    -533,  -533,  -533,     0,     0,  -533,     0,     0,  -533,  -533,
       0,  -533,  -533,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -533,     0,     0,  -533,  -533,     0,  -533,  -533,     0,
    -533,  -533,  -533,     0,  -533,  -533,  -533,  -533,  -533,  -533,
       0,     0,  -533,     0,     0,     0,     0,     0,     0,  -534,
    -534,  -534,  -534,  -534,  -534,  -534,  -534,  -534,     0,     0,
       0,     0,  -533,  -533,  -533,  -534,     0,  -534,  -534,  -534,
    -534,  -533,     0,     0,     0,     0,  -534,  -534,  -534,  -534,
    -534,  -534,  -534,     0,     0,  -534,     0,     0,     0,     0,
       0,     0,     0,     0,  -534,  -534,  -534,  -534,  -534,  -534,
    -534,  -534,  -534,     0,  -534,  -534,  -534,     0,     0,  -534,
       0,     0,  -534,  -534,     0,  -534,  -534,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -534,     0,     0,  -534,  -534,
       0,  -534,  -534,     0,  -534,  -534,  -534,     0,  -534,  -534,
    -534,  -534,  -534,  -534,     0,     0,  -534,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -534,  -534,  -534,     0,
       0,     0,     0,     0,     0,  -534,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
       0,     0,     0,   146,   147,   148,   219,   220,   221,   222,
     153,   154,   155,     0,     0,     0,     0,     0,   156,   157,
     158,   223,   224,   225,   226,   163,   303,   304,   227,   305,
       0,     0,     0,     0,     0,     0,   306,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,   307,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,   182,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,   192,     0,     0,     0,     0,
       0,   193,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,     0,     0,     0,   146,
     147,   148,   219,   220,   221,   222,   153,   154,   155,     0,
       0,     0,     0,     0,   156,   157,   158,   223,   224,   225,
     226,   163,   303,   304,   227,   305,     0,     0,     0,     0,
       0,     0,   306,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
       0,     0,     0,     0,     0,   426,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,   192,     0,     0,     0,     0,     0,   193,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,     0,     0,     0,   146,   147,   148,   219,   220,
     221,   222,   153,   154,   155,     0,     0,     0,     0,     0,
     156,   157,   158,   223,   224,   225,   226,   163,     0,     0,
     227,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,   228,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,     0,     0,
       0,     0,     0,   193,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,     0,     0,
       0,   146,   147,   148,   219,   220,   221,   222,   153,   154,
     155,     0,     0,     0,     0,     0,   156,   157,   158,   223,
     224,   225,   226,   163,     0,     0,   227,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,     0,     0,     0,     0,     0,   193,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,     0,   102,   103,
      18,    19,     0,     0,     0,     0,     0,   104,   105,   106,
      23,    24,    25,    26,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   296,     0,     0,   112,
      50,     0,    51,    52,     0,     0,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   113,   102,   103,
      18,    19,     0,     0,   297,     0,     0,   104,   105,   106,
      23,    24,    25,    26,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   296,     0,     0,   112,
      50,     0,    51,    52,     0,     0,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,     0,     0,     0,     0,    15,   113,    16,    17,
      18,    19,     0,     0,   539,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,     0,     0,    49,
      50,     0,    51,    52,     0,    53,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,     0,    62,    63,    64,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,     0,     0,    49,    50,     0,    51,    52,     0,    53,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,    63,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   246,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,   112,    50,     0,
      51,    52,     0,     0,  -255,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,   233,     0,     0,   234,
     235,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    62,   249,    64,    15,     0,
     102,   103,    18,    19,     0,     0,     0,     0,     0,   104,
     105,   106,    23,    24,    25,    26,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
     246,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
     233,     0,     0,   234,   235,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     249,    64,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,   246,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   112,    50,     0,    51,    52,
       0,   247,   248,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    62,   249,    64,    15,     0,   102,   103,
      18,    19,     0,     0,     0,     0,     0,   104,   105,   106,
      23,    24,    25,    26,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   246,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,     0,     0,   112,
      50,     0,    51,    52,     0,   647,   248,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    62,   249,    64,
      15,     0,   102,   103,    18,    19,     0,     0,     0,     0,
       0,   104,   105,   106,    23,    24,    25,    26,     0,     0,
     107,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   246,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,     0,     0,   112,    50,     0,    51,    52,     0,   247,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   249,    64,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,   112,    50,     0,
      51,    52,     0,   533,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    62,   249,    64,    15,     0,
     102,   103,    18,    19,     0,     0,     0,     0,     0,   104,
     105,   106,    23,    24,    25,    26,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   247,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     249,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,   246,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   112,    50,     0,    51,    52,
       0,   647,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    62,   249,    64,    15,     0,   102,   103,
      18,    19,     0,     0,     0,     0,     0,   104,   105,   106,
      23,    24,    25,    26,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,     0,     0,   112,
      50,     0,    51,    52,     0,   533,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    62,   249,    64,
      15,     0,   102,   103,    18,    19,     0,     0,     0,     0,
       0,   104,   105,   106,    23,    24,    25,    26,     0,     0,
     107,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,     0,     0,   112,    50,     0,    51,    52,     0,   801,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   249,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,   112,    50,     0,
      51,    52,     0,   647,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    62,   249,    64,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
      63,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   112,    50,     0,    51,    52,
       0,     0,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    62,   249,    64,    15,     0,   102,   103,
      18,    19,     0,     0,     0,     0,     0,   104,   105,   106,
      23,    24,    25,    26,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   246,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,     0,     0,   112,
      50,     0,    51,    52,     0,     0,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    62,   249,    64,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
     107,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,     0,     0,   112,    50,     0,    51,    52,     0,     0,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    62,   249,    64,    15,     0,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   108,    35,    36,    37,
     109,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,   111,     0,     0,   112,    50,     0,
      51,    52,     0,     0,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   113,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   212,     0,     0,    49,    50,     0,
      51,    52,     0,    53,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   113,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   296,     0,     0,   343,    50,     0,
      51,    52,     0,   344,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   113,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   108,    35,    36,    37,
     109,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,   112,    50,     0,
      51,    52,     0,     0,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   113,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   296,     0,     0,   343,    50,     0,
      51,    52,     0,     0,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   113,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   876,     0,     0,   112,    50,     0,
      51,    52,     0,     0,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   113,   102,   103,    18,    19,
       0,     0,     0,     0,     0,   104,   105,   106,    23,    24,
      25,    26,     0,     0,   107,     0,     0,   711,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,     0,     0,   360,
     361,     0,     0,     0,   896,     0,     0,   112,    50,     0,
      51,    52,     0,     0,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,   585,   586,
       0,   362,   587,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,     0,     0,   113,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   182,   183,
     184,   185,   186,   187,   188,   189,   190,     0,   191,   192,
     606,   578,     0,     0,   607,   193,   260,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,   192,   591,   586,     0,     0,   592,   193,   260,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,   622,   578,     0,     0,   623,   193,
     260,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,   182,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,   192,   625,   586,     0,     0,
     626,   193,   260,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,   632,   578,
       0,     0,   633,   193,   260,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   182,   183,
     184,   185,   186,   187,   188,   189,   190,     0,   191,   192,
     635,   586,     0,     0,   636,   193,   260,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,   192,   669,   578,     0,     0,   670,   193,   260,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,   672,   586,     0,     0,   673,   193,
     260,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,   182,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,   192,   806,   578,     0,     0,
     807,   193,   260,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,   809,   586,
       0,     0,   810,   193,   260,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   182,   183,
     184,   185,   186,   187,   188,   189,   190,     0,   191,   192,
     956,   578,     0,     0,   957,   193,   260,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,   192,   963,   578,     0,     0,   964,   193,   260,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,   966,   586,     0,     0,   967,   193,
     260,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,   182,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,   192,   591,   586,     0,     0,
     592,   193,   260,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,     0,     0,   360,
     361,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,     0,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
       0,   362,     0,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   356,   357,   358,   359,     0,   237,   360,   361,
       0,   347,   348,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   359,     0,     0,   360,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,     0,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372,     0,     0,     0,     0,     0,     0,   362,  -257,
     363,   364,   365,   366,   367,   368,   369,   370,   371,   372,
       0,     0,     0,     0,     0,     0,     0,  -258,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,     0,     0,   360,   361,     0,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,     0,
       0,   360,   361,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,   363,   364,   365,
     366,   367,   368,   369,   370,   371,   372,     0,     0,     0,
       0,     0,     0,   362,  -259,   363,   364,   365,   366,   367,
     368,   369,   370,   371,   372,     0,     0,     0,     0,     0,
       0,     0,  -260,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   356,   357,   358,   359,     0,     0,   360,   361,
       0,     0,     0,   438,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,     0,     0,   360,
     361,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,     0,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   356,   357,  -560,  -560,     0,     0,   360,   361,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372
};

static const yytype_int16 yycheck[] =
{
       2,    84,    10,    27,    82,    83,   209,    15,    22,    63,
     420,    77,    16,    17,    14,   444,    20,   256,   429,    21,
      16,    17,     5,     6,    20,    15,    53,   302,    28,     2,
      13,     4,     7,   346,   283,    28,     7,   373,   287,   297,
       4,   377,    49,    14,   380,   384,   385,    51,    52,    51,
      52,   483,   111,   684,    16,    17,   302,    28,    20,   314,
      16,    17,   776,   399,    20,    66,   700,    21,    22,   618,
      53,    57,    58,    59,    60,   254,    66,   413,   627,   415,
      55,    66,   290,   684,    55,    99,   294,   688,   424,    51,
      52,   688,    16,    17,    77,    51,    20,   441,    26,   700,
     610,   774,   446,   105,    26,   594,   595,   539,   375,   700,
     289,   290,    29,    49,   624,   294,    26,     0,    88,   878,
      25,    16,   860,    25,   634,   461,    25,   205,    57,    71,
     279,   398,   346,    25,    25,   315,   139,   215,   318,   134,
     320,   144,   322,   590,   324,    99,   593,   414,   137,    55,
     486,   105,   106,    25,    88,   144,   423,    25,    88,    15,
      18,   671,    20,   137,   611,   400,   120,   101,    37,    38,
     384,   385,    25,   143,    58,    59,   112,    51,    88,    53,
      54,    55,    56,   856,   439,   127,   128,   129,    28,   444,
     119,   101,   109,   952,   343,    88,   910,   275,   447,    26,
      90,   792,   210,   211,   453,   117,   134,   945,   719,   143,
      66,   450,   134,   143,   725,   464,   111,   139,   485,   114,
     115,   299,   497,    88,   134,   483,   136,   656,    88,   139,
      25,   297,   137,   143,   139,   137,   867,   238,   137,   912,
     874,   730,   137,   874,   878,   137,   137,   142,   238,   144,
     143,   497,   254,   228,   256,   245,   260,   228,   260,   514,
     264,    88,   863,   137,   260,   137,   863,    16,   264,   137,
     449,   944,   279,   874,   101,   144,   708,   878,   143,    88,
     281,   539,   117,   143,   137,    79,   139,   341,    90,   538,
     111,   281,   346,   114,   115,   121,   281,    88,   808,    88,
      88,    88,   264,   852,   617,   278,   140,   134,   264,   136,
     144,   655,   139,   286,   297,   117,   143,   344,   952,   140,
     141,   307,   117,   144,   332,   333,   334,   335,   122,   139,
     384,   385,   512,    88,   143,   137,   343,   273,   397,   139,
     264,   331,   137,   279,   139,   140,   101,   203,    90,   144,
     760,   952,   143,   209,   143,   143,   143,   536,   331,    90,
     117,   344,   111,   336,   613,   114,   115,   331,   614,    55,
     605,   375,   117,   375,    90,   117,   712,   716,   714,   375,
     137,   136,   238,   722,   723,   341,   117,    88,   143,   245,
     346,   646,    55,   142,   398,   144,   398,   137,    25,   654,
     101,   656,   398,   137,   144,   407,   377,   343,    20,   380,
     414,   650,   414,   137,   137,   481,   665,   483,   414,   423,
     144,   423,    16,   377,   853,   281,    57,   423,   399,   419,
     420,   680,    51,   289,   290,   136,    55,   441,   294,   642,
     444,   852,   143,   137,   415,   399,   713,   449,   450,   134,
     708,    90,   140,   424,   700,   965,   458,   457,   666,    16,
      71,   415,   737,   642,   457,   144,   701,   117,   472,   141,
     424,   549,    55,   539,   410,   331,   432,   467,   117,   441,
     819,   485,   418,   485,    90,   458,   457,   666,   135,   485,
     461,   737,   428,   728,   467,   137,   710,   137,   481,    90,
     483,   503,   716,   738,   506,    25,   508,   461,   722,   723,
     472,   117,    90,    90,    90,   486,    90,   111,    88,    61,
     114,   115,    64,    65,   773,    90,   117,    71,    90,   768,
     532,   101,   486,    51,    71,    53,    54,    55,    56,   117,
     117,   117,   566,   117,    97,   780,   137,    15,   142,    13,
     144,    90,   117,    63,   111,   117,   539,   114,   115,    88,
     584,   137,    25,   419,   420,    16,   136,    25,   582,   139,
     112,   113,   101,   143,    92,   137,   590,   513,   117,   593,
     506,    99,   508,    15,   833,   142,   610,   144,   590,   137,
     836,   593,   594,   595,    99,   830,   604,   117,   853,   137,
     624,    27,   135,   939,    90,   819,   124,   136,   140,   611,
     634,   467,   137,   603,   143,   617,   618,   137,   620,     2,
     140,     4,     5,     6,   144,   627,     9,    10,   582,   637,
      13,   117,    15,    16,    17,   838,   590,    20,   638,   593,
     615,   844,   708,   726,   615,   638,   117,   671,   650,   492,
     137,   655,   656,   137,   117,   609,   710,   611,   682,   117,
     895,   617,   716,   717,    26,    88,    49,   638,   722,   723,
      53,   938,    90,    51,   137,   137,   700,   140,   101,   137,
      63,   144,   140,    37,    38,   111,   144,   111,   114,   115,
     114,   115,    51,   655,    77,   137,    26,    88,    44,   117,
      58,    59,    88,    51,   117,    53,    54,    55,    56,   713,
     101,   713,    15,   136,    18,   101,   142,   713,   144,   137,
     143,    88,   384,   385,   135,   708,    88,   110,   730,   112,
      63,    64,    65,    15,   101,   135,   709,    17,    18,   101,
     748,   743,   732,   714,   746,   136,    88,   603,   137,   739,
     136,   135,   143,   141,   416,   417,    15,   143,    88,   101,
     714,    14,    91,    15,    15,   819,   768,   137,   792,   136,
     760,   101,   134,   137,   136,   140,   143,   139,    90,   112,
     113,   143,   784,   785,   808,   787,   642,   789,   790,   137,
     142,   122,   794,   729,   136,   797,   798,   137,    88,   137,
     462,   143,   137,   137,   134,   117,   136,    15,    15,   745,
     666,   101,   795,   143,   137,   137,   137,   200,    88,    88,
     898,   757,   758,    15,   135,   137,   135,   210,   211,   765,
      15,   101,   101,   676,   677,    15,   137,    61,    15,    88,
      64,    65,    55,    71,   688,   781,   136,   691,   122,   853,
     852,   135,   101,   143,   837,    15,   700,    55,   784,   785,
     137,   787,    88,   789,   790,   137,   136,   136,   794,   137,
     137,   797,   798,   143,   143,   101,   732,   260,    15,   137,
      15,   264,   139,   739,   139,   268,   269,   136,   112,   113,
     273,   915,   137,   829,   143,   278,   279,   125,   126,   127,
     128,   129,   458,   286,   760,   841,    62,    13,    64,    65,
     136,     6,   111,   941,   297,   114,   115,   143,   920,   921,
     922,   923,   111,   925,   926,   114,   115,   929,   676,   931,
     932,   774,   945,   776,   938,   727,   938,   314,   940,   941,
     240,   965,   938,   142,     7,   144,   940,   836,   331,   332,
     333,   334,   335,   336,   337,   338,   112,   113,   341,   874,
     343,   344,   684,   346,    -1,   901,    -1,   903,   939,    -1,
     906,   973,   974,   975,   976,   977,    -1,    -1,    62,    -1,
      64,    65,   838,   985,    -1,   939,    -1,    -1,   844,    -1,
      -1,    -1,   375,    -1,   920,   921,   922,   923,    -1,   925,
     926,   384,   385,   929,    -1,   931,   932,    63,    64,    65,
      -1,    -1,    -1,   856,   857,   398,    -1,   400,   401,   863,
      -1,   865,    -1,    71,    -1,   869,    -1,   410,   112,   113,
      -1,   414,    -1,    -1,   878,   418,   880,    -1,    86,    87,
     423,    -1,    -1,    -1,    -1,   428,    -1,   973,   974,   975,
     976,   977,    -1,    -1,   716,   717,   112,   113,    -1,   985,
     722,   723,    -1,    -1,    63,    64,    65,   910,    51,   912,
      53,    54,    55,    56,    -1,   458,   124,   125,   126,   127,
     128,   129,    -1,    51,   467,    53,    54,    55,    56,   751,
     752,    -1,   754,   755,    -1,    -1,    71,    -1,   481,    -1,
     483,   944,   485,    -1,   948,     0,    -1,    -1,   952,    92,
     954,    86,    87,   112,   113,   959,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    63,    64,    65,    -1,
     513,    26,    27,    -1,    -1,    16,    17,   981,    -1,    20,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
     125,   126,   127,   128,   129,    -1,   539,   819,    63,    64,
      65,    63,    64,    65,    -1,    46,    47,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,   112,   113,    40,    41,    42,
      43,    44,    63,    64,   846,     9,    10,    -1,    -1,    -1,
      -1,    15,    -1,    88,    -1,    -1,    -1,    -1,    -1,    44,
      -1,    63,    64,    65,    -1,    -1,   101,   112,   113,    -1,
     112,   113,    63,    64,    65,    -1,   111,    -1,    -1,   114,
     115,   604,   605,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,   134,
     135,    86,    87,    -1,   139,   140,    -1,   142,   143,   144,
     112,   113,    -1,     2,   637,     4,     5,     6,    -1,    -1,
      -1,   112,   113,    -1,    13,    51,    -1,    53,    54,    55,
      56,    -1,    -1,   118,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   110,    -1,    -1,    -1,
      -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    -1,    53,    -1,    92,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,    -1,    -1,    -1,   701,    -1,
      -1,    -1,    -1,    -1,    -1,   708,   709,   710,    77,    -1,
     713,    -1,    -1,   716,   717,    -1,    -1,    -1,   124,   722,
     723,   127,    -1,    -1,    -1,   728,   729,    -1,    51,    -1,
      53,    54,    55,    56,    -1,   738,    -1,    -1,   144,    -1,
      -1,    -1,   745,   112,    -1,   748,   247,   248,   249,    -1,
      -1,    -1,    -1,    -1,   757,   758,    -1,    -1,    -1,   260,
      -1,    -1,   765,   264,    -1,    -1,   210,   211,     2,    92,
       4,     5,     6,     7,    -1,    98,    99,   780,   781,    13,
      -1,    51,    -1,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,   795,    -1,    51,    -1,    53,    54,    55,    56,
      -1,   124,    -1,    -1,   127,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,   819,    -1,    -1,    53,
      -1,    -1,    92,    -1,   268,   269,   829,   830,    98,    -1,
      -1,   200,    -1,    -1,   837,    92,    -1,    -1,   841,    -1,
     341,    98,    -1,    77,    -1,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,   360,
     361,   362,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372,    -1,    -1,   375,    -1,    -1,    51,   112,    53,
      54,    55,    56,   384,   385,    -1,    -1,    -1,   332,   333,
     334,   335,   895,   337,   338,    -1,    -1,   398,   901,    -1,
     903,    -1,    -1,   906,   273,    -1,    -1,    -1,    -1,   278,
     279,   412,    -1,   414,    -1,   416,   417,   286,    92,    -1,
      -1,    -1,   423,    -1,    98,    99,    -1,    -1,   297,    -1,
      -1,   432,    -1,    -1,   435,   938,    71,   438,    -1,    -1,
     441,    -1,   443,   444,    -1,    -1,    -1,    -1,    -1,    -1,
     124,    86,    87,   127,    -1,    -1,    -1,   401,    -1,    -1,
      -1,   462,   331,    -1,    -1,   139,   200,   336,    -1,    -1,
      -1,   472,    -1,    -1,   343,   344,    -1,   346,    51,    -1,
      53,    54,    55,    56,   485,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   384,   385,    -1,    -1,    92,
      -1,   522,   523,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,   400,   533,    -1,    -1,    -1,    -1,    -1,    -1,   273,
      -1,   410,    -1,    -1,   278,   279,    -1,    -1,    -1,   418,
      -1,   124,   286,    -1,   127,    -1,    -1,    -1,    -1,   428,
      -1,    -1,    -1,   297,    -1,    -1,   139,    -1,    -1,    -1,
      -1,    -1,    -1,     2,    -1,     4,    71,    72,    73,    74,
      75,    76,    77,    78,    13,    80,    81,    -1,    -1,   458,
      -1,    86,    87,    -1,    -1,    -1,    -1,   331,   467,    -1,
      -1,    -1,   336,    -1,    -1,    -1,    -1,    -1,    -1,   343,
     344,    -1,   481,    -1,   483,    -1,   617,    -1,    -1,    -1,
      49,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   513,    -1,   647,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   655,   656,    -1,    -1,    -1,    -1,
     604,    -1,    -1,    -1,    -1,    -1,   400,    -1,    -1,    -1,
     539,    -1,    -1,    -1,    -1,    -1,   410,    -1,    -1,    -1,
      -1,    -1,    -1,   112,   418,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   637,   428,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   705,    -1,    -1,    -1,    -1,   710,
     711,    -1,   713,    -1,    -1,   716,   717,    -1,    -1,    -1,
      -1,   722,   723,    -1,   458,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   467,    -1,    -1,   605,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   481,    -1,   483,
     751,   752,    -1,   754,   755,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   764,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   200,     2,    -1,     4,    -1,    -1,    -1,    -1,   513,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     801,    -1,    -1,    -1,   748,   539,    -1,    -1,    -1,    -1,
     811,   812,    -1,    -1,    -1,    -1,    -1,    -1,   819,    49,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,   708,
     709,   710,    -1,    -1,   273,   846,    -1,   716,    -1,   278,
     279,    -1,   853,   722,   723,    -1,    -1,   286,    -1,   728,
     729,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   738,
      -1,   605,    -1,    -1,    -1,    -1,   745,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,   757,   758,
      -1,    -1,    -1,    -1,    -1,    -1,   765,    -1,    -1,    -1,
      -1,    -1,   331,    -1,    -1,    -1,    -1,   336,    -1,    -1,
      -1,   780,   781,    -1,   343,    -1,    -1,   346,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     819,    -1,    -1,    -1,    -1,   384,   385,    -1,    -1,    -1,
     829,   830,    -1,    -1,    -1,    -1,    -1,   701,   837,    -1,
     200,   400,   841,    -1,   708,   709,    -1,    -1,    -1,    -1,
      -1,   410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,
      -1,    -1,    -1,    -1,   728,   729,    -1,    -1,    -1,   428,
      -1,    -1,    -1,    -1,   738,    -1,    -1,    -1,    -1,    -1,
      -1,   745,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   757,   758,    -1,   895,    -1,    -1,   458,
      -1,   765,   901,    -1,   903,    -1,    -1,   906,   467,    -1,
      -1,    -1,    -1,   273,    -1,    -1,   780,   781,   278,   279,
      -1,    -1,    -1,    51,    52,    -1,   286,    55,    -1,    -1,
      -1,   795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,   513,    -1,    84,    85,    86,    87,
      -1,    -1,    -1,    -1,    -1,   829,   830,    -1,    -1,    -1,
      98,   331,    -1,   837,    -1,    -1,   336,   841,    -1,    -1,
      -1,    -1,    -1,   343,    -1,    -1,   346,    -1,    -1,    -1,
      -1,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,   131,   132,    -1,    -1,    -1,    -1,    -1,
     138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   384,   385,    -1,    -1,    -1,    -1,
      -1,   895,    -1,    -1,    -1,    -1,    -1,   901,    -1,   903,
     400,    -1,   906,    -1,    -1,    -1,   605,    -1,    -1,    -1,
     410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,    -1,
      71,    72,    73,    74,    75,    76,    77,    -1,   428,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    -1,    -1,    -1,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   458,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   467,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
      -1,    46,    47,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     709,   710,    77,   513,    -1,    -1,    -1,   716,    -1,    -1,
      -1,    -1,    -1,   722,   723,    -1,    -1,    -1,    -1,   728,
     729,    -1,    -1,    -1,    -1,     0,    -1,    -1,    -1,   738,
      -1,    -1,    -1,    -1,    -1,    -1,   745,    -1,    13,    14,
      15,    -1,    17,    18,    -1,    20,    -1,    -1,   757,   758,
      -1,    26,    -1,    -1,    -1,    -1,   765,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,   780,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   605,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
     819,    86,    87,    88,    -1,    90,    -1,    -1,    -1,    -1,
     829,   830,    -1,    -1,    -1,    -1,   101,    -1,   837,    -1,
      -1,    -1,   841,    -1,    -1,    -1,   111,    -1,    -1,   114,
     115,    -1,   117,   118,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,    -1,   142,   143,   144,
      -1,    -1,   247,   248,   249,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,
      -1,   701,   901,    -1,   903,    -1,    -1,   906,    -1,   709,
     710,    -1,    -1,    -1,    -1,    -1,   716,    -1,    -1,    -1,
      -1,    -1,   722,   723,    -1,    -1,    -1,    -1,   728,   729,
      -1,    -1,   297,    -1,    -1,    -1,    -1,    -1,   738,    -1,
      -1,    -1,    -1,    -1,    -1,   745,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,   758,    -1,
      -1,    -1,    -1,    -1,    -1,   765,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   344,
     780,   781,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,   819,
      -1,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,   829,
     830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   841,    -1,    -1,    -1,    -1,    -1,   412,    -1,    -1,
      -1,   416,   417,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   432,    -1,    -1,
     435,    -1,    -1,   438,    -1,    -1,   441,    -1,   443,   444,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   895,    -1,   462,    -1,    -1,
      -1,   901,    -1,   903,    -1,    -1,   906,   472,    -1,    -1,
      -1,     0,    -1,    -1,    -1,    -1,   481,    -1,   483,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    -1,   504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,   522,   523,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   533,    -1,
      -1,    -1,    -1,    -1,   539,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    88,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,   114,   115,    -1,   117,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,   617,   142,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,     0,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   647,    21,    22,    23,    24,    -1,    -1,    -1,
     655,   656,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     705,    -1,    -1,   708,    -1,    -1,   711,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,   100,    -1,   102,   103,   104,   105,   106,   107,
      -1,    -1,   110,   111,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,     0,    -1,    -1,   751,   752,    -1,   754,
     755,    -1,   130,   131,   132,    -1,    13,    14,    15,   764,
      17,    18,    -1,    20,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
     795,    -1,    -1,    -1,    -1,    -1,   801,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   811,   812,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    88,   837,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   846,    -1,    -1,   101,    -1,    -1,    -1,   853,    -1,
      -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,    -1,
     117,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,     0,    -1,    -1,   135,   136,
     137,    -1,    -1,   140,    -1,   142,   143,   144,    13,    14,
      15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    88,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,
     115,    -1,   117,   118,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,
     135,   136,   137,    -1,    -1,   140,    -1,   142,   143,   144,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    -1,    -1,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
     111,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
     131,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,     1,   144,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    -1,    17,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,   100,    -1,   102,   103,   104,   105,   106,   107,    -1,
      -1,   110,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,   131,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,     1,   144,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    -1,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,   100,    -1,   102,   103,   104,   105,   106,
     107,    -1,    -1,   110,   111,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,   131,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,     1,   144,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,   111,    -1,    -1,   114,
     115,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,   130,   131,   132,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,   142,    -1,   144,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
     100,    -1,   102,   103,   104,   105,   106,   107,    -1,    -1,
     110,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,   131,   132,    -1,    -1,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,     1,   144,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    14,    15,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,   100,    -1,   102,   103,   104,   105,   106,   107,
      -1,    -1,   110,   111,    -1,    -1,   114,   115,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   130,   131,   132,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,   142,    -1,   144,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,   111,    -1,
      -1,   114,   115,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   130,   131,   132,
      -1,    19,    -1,    21,    22,    23,    24,   140,    -1,   142,
      -1,   144,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,   100,    -1,   102,   103,   104,   105,   106,   107,
      -1,    -1,   110,   111,    -1,    -1,   114,   115,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   130,   131,   132,    -1,    19,    -1,    21,    22,
      23,    24,   140,    -1,   142,    -1,   144,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,   111,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
       1,   144,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    -1,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
     111,    -1,    -1,   114,   115,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   130,
     131,   132,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   142,    -1,   144,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,   100,    -1,   102,   103,   104,   105,
     106,   107,    -1,    -1,   110,   111,    -1,    -1,   114,   115,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,   130,   131,   132,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   142,    -1,   144,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
     111,    -1,    -1,   114,   115,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,   130,
     131,   132,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   142,    -1,   144,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    -1,    -1,   100,    -1,   102,   103,   104,   105,
     106,   107,    -1,    -1,   110,   111,    -1,    -1,   114,   115,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,   130,   131,   132,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   142,    -1,   144,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
     131,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   144,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    -1,    -1,    -1,    -1,    -1,   138,
     139,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    99,   100,    -1,
     102,   103,   104,   105,   106,   107,    -1,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   130,   131,
     132,    19,   134,    21,    22,    23,    24,   139,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    99,   100,    -1,   102,   103,   104,   105,   106,   107,
      -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   130,   131,   132,    19,   134,    21,    22,    23,
      24,   139,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    99,   100,    -1,   102,   103,
     104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   130,   131,   132,    19,
     134,    21,    22,    23,    24,   139,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    99,
     100,    -1,   102,   103,   104,   105,   106,   107,    -1,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     130,   131,   132,    19,   134,    21,    22,    23,    24,   139,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    90,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    99,   100,    -1,   102,   103,   104,   105,
     106,   107,    -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,   131,   132,    -1,    -1,    -1,
      -1,    -1,    -1,   139,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    90,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      99,   100,    -1,   102,   103,   104,   105,   106,   107,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,   131,   132,    -1,    -1,    -1,    -1,    -1,    -1,
     139,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    99,   100,    -1,
     102,   103,   104,   105,   106,   107,    -1,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   130,   131,
     132,    19,    -1,    21,    22,    23,    24,   139,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    99,   100,    -1,   102,   103,   104,   105,   106,   107,
      -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   130,   131,   132,    19,    -1,    21,    22,    23,
      24,   139,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    99,   100,    -1,   102,   103,
     104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,    -1,
      -1,    -1,    -1,    -1,    -1,   139,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,   131,   132,    -1,    -1,    -1,    -1,
      -1,   138,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    -1,    -1,    -1,    -1,    -1,   138,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    -1,    -1,
      -1,    -1,    -1,   138,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    -1,    -1,    -1,    -1,    -1,   138,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   130,    21,    22,
      23,    24,    -1,    -1,   137,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   130,    21,    22,
      23,    24,    -1,    -1,   137,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,   130,   131,   132,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,   100,    -1,   102,   103,   104,   105,   106,   107,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   130,   131,   132,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    99,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,   111,    -1,    -1,   114,
     115,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   130,   131,   132,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
     111,    -1,    -1,   114,   115,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   130,
     131,   132,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    99,   100,    -1,   102,   103,   104,   105,   106,
     107,    -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   130,   131,   132,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    99,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   130,   131,   132,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,   100,    -1,   102,   103,   104,   105,   106,   107,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   130,   131,   132,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   130,   131,   132,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   130,
     131,   132,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,   100,    -1,   102,   103,   104,   105,   106,
     107,    -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   130,   131,   132,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   130,   131,   132,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,   100,    -1,   102,   103,   104,   105,   106,   107,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   130,   131,   132,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   130,   131,   132,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,
      -1,   102,   103,   104,   105,   106,   107,    -1,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   130,
     131,   132,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    -1,    -1,   100,    -1,   102,   103,   104,   105,   106,
     107,    -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   130,   131,   132,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,   100,    -1,   102,
     103,   104,   105,   106,   107,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   130,   131,   132,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,
      -1,   100,    -1,   102,   103,   104,   105,   106,   107,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   130,   131,   132,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   130,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   130,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   130,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   130,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   130,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   130,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    44,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,    -1,   102,   103,   104,
     105,   106,   107,    -1,    -1,   110,    -1,    -1,    51,    52,
      -1,   118,    55,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,   130,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,   131,   132,
      51,    52,    -1,    -1,    55,   138,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    51,    52,    -1,    -1,    55,   138,   139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    51,    52,    -1,    -1,    55,   138,
     139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,   131,   132,    51,    52,    -1,    -1,
      55,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    51,    52,
      -1,    -1,    55,   138,   139,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,   131,   132,
      51,    52,    -1,    -1,    55,   138,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    51,    52,    -1,    -1,    55,   138,   139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    51,    52,    -1,    -1,    55,   138,
     139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,   131,   132,    51,    52,    -1,    -1,
      55,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    51,    52,
      -1,    -1,    55,   138,   139,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,   131,   132,
      51,    52,    -1,    -1,    55,   138,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    51,    52,    -1,    -1,    55,   138,   139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    51,    52,    -1,    -1,    55,   138,
     139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,   131,   132,    51,    52,    -1,    -1,
      55,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    -1,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,   144,    86,    87,
      -1,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,    -1,    -1,    -1,   118,   137,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   118,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,    -1,
      -1,    -1,    -1,   118,   137,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      -1,    -1,    -1,    91,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   146,   147,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    66,    67,    69,    70,    89,    92,
      93,    95,    96,    98,   100,   102,   103,   104,   105,   106,
     107,   110,   130,   131,   132,   148,   149,   150,   155,   157,
     159,   161,   162,   165,   166,   168,   169,   170,   172,   173,
     182,   196,   217,   236,   237,   247,   248,   252,   253,   254,
     260,   261,   262,   264,   265,   266,   267,   268,   269,   293,
     307,   150,    21,    22,    30,    31,    32,    39,    51,    55,
      86,    89,    92,   130,   174,   175,   196,   217,   266,   269,
     293,   175,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    45,    46,    47,    48,
      49,    50,    51,    52,    55,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    80,    81,    84,    85,    86,    87,
      98,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   131,   132,   138,   139,   176,   180,   181,   268,   288,
     197,    89,   159,   160,   173,   217,   266,   267,   269,   160,
     203,   205,    89,   166,   173,   217,   222,   266,   269,    33,
      34,    35,    36,    48,    49,    50,    51,    55,   102,   176,
     177,   178,   262,   111,   114,   115,   142,   144,   160,   256,
     257,   258,   299,   304,   305,   306,    51,    98,    99,   131,
     165,   182,   188,   191,   194,   248,   291,   292,   188,   188,
     139,   185,   186,   189,   190,   307,   185,   189,   139,   300,
     305,   177,   151,   134,   182,   217,   182,    55,     1,    92,
     153,   154,   155,   167,   168,   307,   198,   200,   183,   194,
     291,   307,   182,   290,   291,   307,    89,   137,   172,   217,
     266,   269,   201,    53,    54,    56,    63,   106,   176,   263,
      62,    64,    65,   112,   113,   249,   250,    63,   249,    63,
     249,    63,   249,    61,   249,    58,    59,   161,   182,   182,
     299,   306,    40,    41,    42,    43,    44,    37,    38,    28,
     234,   117,   137,    92,    98,   169,   117,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      86,    87,   118,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    88,   101,   136,   143,   297,    88,   297,
     298,    26,   134,   238,    90,    90,   185,   189,   238,   159,
      51,    55,   174,    58,    59,   121,   270,    88,   136,   297,
     212,   289,   213,    88,   143,   296,   152,   153,    55,    16,
     218,   304,   117,    88,   136,   297,    90,    90,   218,   160,
     160,    55,    88,   136,   297,    25,   106,   137,   259,   299,
     111,   258,    20,   241,   304,    57,   182,   182,    91,   137,
     192,   193,   307,    57,   137,   192,   193,   187,   188,   194,
     291,   307,   188,   159,   300,   301,   159,   156,   134,   153,
      88,   297,    90,   155,   167,   140,   299,   306,   301,   155,
     301,   141,   193,   303,   305,   193,   303,   135,   303,    55,
     169,   170,   171,   137,    88,   136,   297,    51,    53,    54,
      55,    56,    92,    98,    99,   124,   127,   139,   232,   273,
     274,   275,   276,   277,   278,   279,   282,   283,   284,   285,
     286,    63,   249,   251,   255,   256,    62,   250,    63,    63,
      63,    61,    71,    71,   150,   160,   160,   160,   160,   155,
     159,   159,   235,    98,   161,   182,   194,   195,   167,   137,
     172,   137,   157,   158,   161,   173,   182,   184,   195,   217,
     269,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,    51,    52,    55,
     180,   185,   294,   295,   187,    51,    52,    55,   180,   185,
     294,    51,    55,   294,   240,   239,   158,   182,   184,   158,
     184,    97,   163,   210,   271,   209,    51,    55,   174,   294,
     187,   294,   152,   159,   214,   215,    15,    13,   243,   307,
     153,    16,    51,    55,   187,    51,    55,   153,    27,   219,
     304,   219,    51,    55,   187,    51,    55,   207,   179,   153,
     241,   182,   194,    15,   182,   182,   255,    98,   182,   191,
     291,   182,   292,   301,   137,   193,   137,   301,   140,   177,
     148,   135,   184,   301,   155,   199,   291,   169,   171,    51,
      55,   187,    51,    55,   117,    51,    92,    98,   223,   224,
     225,   275,   273,   202,   137,   287,   307,   182,   137,   287,
      51,   137,   287,    51,    63,   153,   256,   182,   182,    79,
     122,   227,   228,   307,   182,   193,   301,   171,   137,    44,
     117,    44,    88,   136,   297,   300,    90,    90,   185,   189,
     300,   302,    90,    90,   186,   189,   186,   189,   227,   227,
     164,   304,   160,   152,   302,    15,   301,   139,   272,   273,
     176,   182,   195,   244,   307,    18,   221,   307,    17,   220,
     221,    90,    90,   302,    90,    90,   221,   204,   206,   302,
     160,   177,   135,    15,   193,   218,   182,   192,   291,   135,
     301,   303,   302,   225,   137,   275,   137,   301,   229,   300,
      29,   109,   233,   276,   282,   284,   286,   277,   279,   284,
     277,   135,   226,   229,   277,   278,   280,   281,   284,   286,
     152,    98,   182,   171,   155,   182,    51,    55,   187,    51,
      55,    57,   119,   158,   184,   161,   184,   163,   141,    90,
     158,   184,   158,   184,   163,   238,   234,   152,   153,   227,
     211,   304,    15,   273,   152,   304,   216,    91,   245,   307,
     153,    14,   246,   307,   160,    15,    90,    15,   153,   153,
     219,   182,   153,   137,   301,   224,   137,    98,   223,   140,
     142,   152,   153,   137,   287,   137,   287,   137,   287,   137,
     287,   287,   229,   122,   137,   287,    89,   217,   137,   287,
     137,   287,    15,   182,   302,   182,   182,   158,   184,    15,
     135,   153,   152,   301,    15,   272,    89,   173,   217,   266,
     269,   218,   153,   218,    15,    15,   208,   221,   241,   242,
     137,   224,   137,   275,    51,   230,   231,   274,    15,   135,
     277,   284,   277,   277,   122,   281,   284,    55,    88,   277,
     280,   284,   277,   135,    15,   152,    55,    88,   136,   297,
     153,   153,   153,   224,   137,   137,   300,   287,   137,   287,
     287,   287,   137,   287,   137,   287,    51,    55,   287,   137,
     287,   287,    15,    51,    55,   187,    51,    55,   243,   220,
      15,   224,   231,   277,   277,   284,   277,   277,   302,   287,
     287,   137,   287,   287,   287,   277,   287
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   145,   147,   146,   148,   149,   149,   149,   149,   150,
     151,   150,   152,   153,   154,   154,   154,   154,   156,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     155,   155,   155,   157,   157,   157,   157,   157,   157,   157,
     157,   158,   158,   158,   159,   159,   159,   159,   159,   159,
     160,   161,   161,   162,   162,   164,   163,   165,   165,   165,
     165,   165,   165,   165,   165,   165,   165,   165,   166,   166,
     167,   167,   168,   168,   168,   168,   168,   168,   168,   168,
     168,   168,   169,   169,   170,   170,   171,   171,   172,   172,
     172,   172,   172,   172,   172,   172,   173,   173,   173,   173,
     173,   173,   173,   173,   174,   174,   175,   175,   175,   176,
     176,   176,   176,   176,   177,   177,   178,   179,   178,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   183,   183,   183,   183,   184,   184,   185,   186,   186,
     187,   187,   187,   187,   187,   188,   188,   188,   188,   188,
     190,   189,   191,   192,   192,   193,   193,   194,   194,   194,
     194,   195,   195,   195,   196,   196,   196,   196,   196,   196,
     196,   196,   197,   196,   198,   199,   196,   200,   196,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   196,   196,
     196,   201,   202,   196,   196,   196,   203,   204,   196,   205,
     206,   196,   196,   196,   207,   208,   196,   209,   196,   210,
     211,   196,   212,   196,   213,   214,   196,   215,   216,   196,
     196,   196,   196,   196,   217,   218,   218,   218,   219,   219,
     220,   220,   221,   221,   222,   222,   223,   223,   224,   224,
     225,   225,   225,   225,   225,   225,   225,   225,   225,   226,
     226,   226,   226,   226,   226,   226,   226,   226,   226,   226,
     226,   226,   226,   226,   227,   227,   228,   228,   228,   229,
     229,   230,   230,   231,   231,   232,   232,   233,   233,   235,
     234,   236,   236,   236,   236,   237,   237,   237,   237,   237,
     237,   237,   237,   237,   239,   238,   240,   238,   241,   242,
     242,   243,   243,   244,   244,   244,   245,   245,   246,   246,
     247,   247,   247,   247,   248,   248,   248,   248,   249,   249,
     250,   251,   250,   250,   250,   252,   252,   253,   253,   254,
     255,   255,   256,   256,   257,   257,   258,   259,   258,   260,
     260,   261,   261,   262,   263,   263,   263,   263,   263,   263,
     264,   264,   265,   265,   265,   265,   266,   266,   266,   266,
     266,   267,   268,   268,   268,   268,   268,   268,   268,   268,
     269,   269,   270,   271,   270,   272,   272,   273,   273,   273,
     273,   273,   273,   273,   273,   273,   273,   273,   273,   273,
     273,   273,   274,   274,   274,   274,   275,   275,   276,   276,
     277,   277,   278,   279,   280,   281,   281,   282,   282,   283,
     283,   284,   284,   285,   285,   286,   287,   287,   288,   289,
     288,   290,   290,   291,   291,   292,   292,   292,   293,   293,
     293,   294,   294,   294,   294,   295,   295,   295,   296,   296,
     297,   297,   298,   298,   299,   299,   300,   300,   301,   302,
     303,   303,   303,   304,   304,   304,   305,   306,   306,   307
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     3,     3,     6,     5,     5,     5,     5,
       3,     1,     3,     1,     1,     3,     3,     3,     2,     1,
       1,     1,     1,     1,     4,     0,     5,     2,     3,     4,
       5,     4,     5,     2,     2,     2,     2,     2,     1,     3,
       1,     3,     1,     2,     3,     5,     2,     4,     2,     4,
       1,     3,     1,     3,     2,     3,     1,     2,     1,     4,
       3,     3,     3,     3,     2,     1,     1,     4,     3,     3,
       3,     3,     2,     1,     1,     1,     2,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       6,     5,     5,     5,     5,     4,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     4,     4,     2,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     6,     6,
       1,     1,     2,     4,     2,     1,     3,     3,     1,     1,
       1,     1,     2,     4,     2,     1,     2,     2,     4,     1,
       0,     2,     2,     2,     1,     1,     2,     1,     2,     3,
       4,     3,     4,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     0,     0,     5,     0,     3,     3,
       3,     2,     3,     3,     1,     2,     4,     3,     2,     1,
       2,     0,     0,     5,     6,     6,     0,     0,     7,     0,
       0,     7,     5,     4,     0,     0,     9,     0,     6,     0,
       0,     8,     0,     5,     0,     0,     7,     0,     0,     9,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     5,     1,     2,     1,     1,     1,     3,     1,     3,
       1,     4,     6,     3,     5,     2,     4,     1,     3,     6,
       8,     4,     6,     4,     2,     6,     2,     4,     6,     2,
       4,     2,     4,     1,     1,     1,     3,     1,     4,     1,
       4,     1,     3,     1,     1,     4,     1,     3,     3,     0,
       5,     2,     4,     5,     5,     2,     4,     4,     3,     3,
       3,     2,     1,     4,     0,     5,     0,     5,     5,     1,
       1,     6,     1,     1,     1,     1,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     2,     3,     1,     2,
       1,     0,     4,     1,     2,     2,     3,     2,     3,     1,
       1,     2,     1,     2,     1,     2,     1,     0,     4,     2,
       3,     1,     4,     2,     1,     1,     1,     1,     1,     2,
       2,     3,     1,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     0,     4,     3,     2,     6,     8,     4,
       6,     4,     6,     2,     4,     6,     2,     4,     2,     4,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     3,     2,     2,     2,     1,     3,     1,     3,     1,
       1,     2,     1,     1,     1,     2,     2,     1,     1,     0,
       4,     1,     2,     1,     3,     3,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     1,     2,     2,
       0,     1,     1,     1,     1,     1,     1,     1,     2,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (p, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, p); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (p);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, p);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, parser_state *p)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              , p);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, p); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, parser_state *p)
{
  YYUSE (yyvaluep);
  YYUSE (p);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (parser_state *p)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, p);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1201 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 5205 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1206 "parse.y" /* yacc.c:1646  */
    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 5214 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1213 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5222 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1219 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5230 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1223 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5239 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1228 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5247 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1232 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5255 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1239 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = local_switch(p);
                    }
#line 5263 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1243 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      (yyval.nd) = 0;
                    }
#line 5273 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1254 "parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[-2].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                        NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                      }
                      else if ((yyvsp[-1].nd)) {
                        yywarn(p, "else without rescue is useless");
                        (yyval.nd) = push((yyvsp[-3].nd), (yyvsp[-1].nd));
                      }
                      else {
                        (yyval.nd) = (yyvsp[-3].nd);
                      }
                      if ((yyvsp[0].nd)) {
                        if ((yyval.nd)) {
                          (yyval.nd) = new_ensure(p, (yyval.nd), (yyvsp[0].nd));
                        }
                        else {
                          (yyval.nd) = push((yyvsp[0].nd), new_nil(p));
                        }
                      }
                    }
#line 5299 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1278 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5307 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1284 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5315 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1288 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5324 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1293 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5332 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1297 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 5340 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1302 "parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_FNAME;}
#line 5346 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1303 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 5354 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1307 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 5362 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1311 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5370 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1315 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5378 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1319 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5386 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1323 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5394 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1327 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5402 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1331 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 5411 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1337 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5419 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1341 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 5427 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1345 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5435 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1349 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 5443 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1356 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5451 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1360 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5459 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1364 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern("[]",2), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5467 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1368 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5475 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1372 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5483 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1376 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 5492 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1381 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5500 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1385 "parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5509 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1393 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5517 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1402 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5525 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1406 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5533 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1410 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 5541 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1414 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 5549 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1421 "parse.y" /* yacc.c:1646  */
    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 5560 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1435 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 5568 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1441 "parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                    }
#line 5576 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1447 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                    }
#line 5585 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1454 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5593 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1458 "parse.y" /* yacc.c:1646  */
    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 5602 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1463 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 5610 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1467 "parse.y" /* yacc.c:1646  */
    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 5619 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1472 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 5627 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1476 "parse.y" /* yacc.c:1646  */
    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 5636 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1481 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 5644 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1485 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 5652 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1489 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 5660 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1493 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 5668 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1497 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 5676 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1503 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 5684 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1507 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5692 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 1514 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5700 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1520 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 5708 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1524 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 5716 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1528 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5724 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1532 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5732 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1536 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 5740 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1540 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 5748 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1544 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 5756 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1548 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5764 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1552 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 5772 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1556 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 5780 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1563 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 5788 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1569 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 5796 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1573 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 5804 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1579 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 5812 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1583 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 5820 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1589 "parse.y" /* yacc.c:1646  */
    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 5828 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1593 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern("[]",2), (yyvsp[-1].nd), '.');
                    }
#line 5836 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1597 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5844 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1601 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 5852 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1605 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5860 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1609 "parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 5870 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1615 "parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 5880 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1621 "parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 5889 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1628 "parse.y" /* yacc.c:1646  */
    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 5897 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1632 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern("[]",2), (yyvsp[-1].nd), '.');
                    }
#line 5905 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1636 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5913 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1640 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 5921 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1644 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5929 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1648 "parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 5939 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1654 "parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 5949 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1660 "parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 5958 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1667 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 5966 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1674 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[0].id)));
                    }
#line 5974 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1678 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[0].id)));
                    }
#line 5982 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1682 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 5991 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1692 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6000 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 1697 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6009 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 1708 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6017 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1711 "parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_FNAME;}
#line 6023 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1712 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6031 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 1717 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('|');   }
#line 6037 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1718 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('^');   }
#line 6043 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1719 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('&');   }
#line 6049 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 1720 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("<=>",3); }
#line 6055 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 1721 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("==",2);  }
#line 6061 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1722 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("===",3); }
#line 6067 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 1723 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("=~",2);  }
#line 6073 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 1724 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("!~",2);  }
#line 6079 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1725 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('>');   }
#line 6085 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1726 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern(">=",2);  }
#line 6091 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1727 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('<');   }
#line 6097 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1728 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("<=",2);  }
#line 6103 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1729 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("!=",2);  }
#line 6109 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1730 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("<<",2);  }
#line 6115 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 1731 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern(">>",2);  }
#line 6121 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1732 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('+');   }
#line 6127 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1733 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('-');   }
#line 6133 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 1734 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('*');   }
#line 6139 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 1735 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('*');   }
#line 6145 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 1736 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('/');   }
#line 6151 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 1737 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('%');   }
#line 6157 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1738 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("**",2);  }
#line 6163 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1739 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('!');   }
#line 6169 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 1740 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('~');   }
#line 6175 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1741 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("+@",2);  }
#line 6181 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 1742 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("-@",2);  }
#line 6187 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1743 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("[]",2);  }
#line 6193 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 1744 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("[]=",3); }
#line 6199 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1745 "parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('`');   }
#line 6205 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 1763 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6213 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 1767 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6221 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 1771 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern("[]",2), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6229 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 1775 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6237 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 1779 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6245 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 1783 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6253 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 1787 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6262 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 1792 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6271 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 1797 "parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6280 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 1802 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6288 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 1806 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6296 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 1810 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 6304 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 1814 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 6312 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 1818 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 6320 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 1822 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 6328 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1826 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 6336 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 1830 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 6344 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 1834 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 6352 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 1838 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 6360 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 1842 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 6368 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 1846 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 6376 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 1850 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 6384 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1854 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 6392 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 1858 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 6400 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1862 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 6408 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1866 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 6416 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 1870 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 6424 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1874 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 6432 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1878 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 6440 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 1882 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 6448 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 1886 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 6456 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 1890 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 6464 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 1894 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 6472 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 1898 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 6480 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1902 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6488 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 1906 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 6496 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1910 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 6504 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 1914 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 6512 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 1918 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6520 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 1922 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6528 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 1926 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 6536 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 1930 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 6544 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 1934 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6552 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1941 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6561 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 1946 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 6569 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 1950 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6578 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 1957 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6586 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 1961 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6596 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 1969 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6604 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 1981 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6613 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1986 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 6622 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 244:
#line 1991 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(list1(new_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6631 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 245:
#line 1998 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6641 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 246:
#line 2004 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6650 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 2009 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(list1(new_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6659 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 2014 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 6668 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 2019 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6677 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 2025 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 6686 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 2030 "parse.y" /* yacc.c:1646  */
    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6695 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 2037 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 6703 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 2043 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6711 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 2047 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 6719 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 257:
#line 2057 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6729 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 258:
#line 2063 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6739 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 259:
#line 2069 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6748 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 2074 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 6757 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 2081 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6766 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 2086 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 6775 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 2091 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 6784 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 2105 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 6792 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 272:
#line 2109 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 6801 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 2115 "parse.y" /* yacc.c:1646  */
    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6810 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 2120 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 6819 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 2124 "parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_ENDARG;}
#line 6825 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 2125 "parse.y" /* yacc.c:1646  */
    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 6834 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 2129 "parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_ENDARG;}
#line 6840 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 2130 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_nil(p);
                    }
#line 6848 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 2134 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6856 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 2138 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6864 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 2142 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6872 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 282:
#line 2146 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6881 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 283:
#line 2151 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6890 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 2156 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 6898 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 285:
#line 2160 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6906 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 286:
#line 2164 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 6914 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 287:
#line 2168 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 6922 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 288:
#line 2172 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 6930 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 2177 "parse.y" /* yacc.c:1646  */
    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6939 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 291:
#line 2182 "parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 6949 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 292:
#line 2188 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 6958 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 293:
#line 2193 "parse.y" /* yacc.c:1646  */
    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 6970 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 2204 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 6979 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 2212 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 6988 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 296:
#line 2216 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 6994 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 2216 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7000 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 2219 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7009 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 299:
#line 2223 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7015 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 300:
#line 2223 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7021 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 301:
#line 2226 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7030 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 302:
#line 2233 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7038 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 2237 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7046 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 2241 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7052 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 2243 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7058 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 2246 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7067 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 307:
#line 2252 "parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                    }
#line 7077 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 308:
#line 2259 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                    }
#line 7087 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 309:
#line 2266 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 7096 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 2271 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      p->in_single = 0;
                    }
#line 7105 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 2277 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 7117 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 2286 "parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                    }
#line 7127 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 313:
#line 2293 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                    }
#line 7137 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 2299 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7146 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 2303 "parse.y" /* yacc.c:1646  */
    {
                      p->in_def++;
                      (yyval.nd) = local_switch(p);
                    }
#line 7155 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 316:
#line 2310 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_def(p, (yyvsp[-5].id), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                      local_resume(p, (yyvsp[-3].nd));
                      p->in_def--;
                      p->cmdarg_stack = (yyvsp[-4].stack);
                    }
#line 7167 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 317:
#line 2318 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_FNAME;
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7177 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 318:
#line 2324 "parse.y" /* yacc.c:1646  */
    {
                      p->in_single++;
                      p->lstate = EXPR_ENDFN; /* force for args */
                      (yyval.nd) = local_switch(p);
                    }
#line 7187 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 319:
#line 2332 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-7].nd), (yyvsp[-4].id), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                      local_resume(p, (yyvsp[-3].nd));
                      p->in_single--;
                      p->cmdarg_stack = (yyvsp[-5].stack);
                    }
#line 7199 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 2340 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 7207 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 2344 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 7215 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 2348 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_redo(p);
                    }
#line 7223 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 323:
#line 2352 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_retry(p);
                    }
#line 7231 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 324:
#line 2358 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 7240 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 2377 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7248 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 2384 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7256 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 2390 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 7264 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 336:
#line 2397 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 7272 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 2401 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), 0);
                    }
#line 7280 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 338:
#line 2407 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7288 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 2411 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7296 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 340:
#line 2417 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 7304 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 2421 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 7312 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 342:
#line 2425 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 7320 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 2429 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-2].nd), (node*)-1, 0);
                    }
#line 7328 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 2433 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (node*)-1, (yyvsp[0].nd));
                    }
#line 7336 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 2437 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 7344 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 2441 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 7352 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 2445 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
#line 7360 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 2449 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[0].nd));
                    }
#line 7368 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 2455 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7376 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 2459 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7384 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 2463 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 7392 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 2467 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7400 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 2471 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7408 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 2475 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, 0);
                    }
#line 7416 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 2479 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7424 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 2483 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].id));
                    }
#line 7432 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 2487 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7440 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 2491 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7448 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 2495 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 7456 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 2499 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7464 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 361:
#line 2503 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7472 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 2507 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7480 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 2511 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].id));
                    }
#line 7488 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 2518 "parse.y" /* yacc.c:1646  */
    {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7497 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 2525 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7505 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 2529 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7513 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 2533 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7521 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 2540 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7529 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 2544 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7537 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 2554 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 7546 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 2562 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7554 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 2566 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7562 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 2572 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7570 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 2576 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7578 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 2582 "parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                    }
#line 7586 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 2588 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                    }
#line 7595 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 2595 "parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[-1].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7609 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 2605 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7617 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 2609 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 7626 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 2614 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 7635 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 2621 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7643 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 2625 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7651 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 2629 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 7659 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 2633 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 7667 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 2637 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), intern("call",4), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 7675 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 2641 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), intern("call",4), (yyvsp[0].nd), tCOLON2);
                    }
#line 7683 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 391:
#line 2645 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 7691 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 2649 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 7699 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 393:
#line 2653 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern("[]",2), (yyvsp[-1].nd), '.');
                    }
#line 7707 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 394:
#line 2659 "parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 7716 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 395:
#line 2665 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                    }
#line 7726 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 396:
#line 2671 "parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 7735 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 397:
#line 2677 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                    }
#line 7745 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 398:
#line 2687 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 7753 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 399:
#line 2693 "parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 7766 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 401:
#line 2707 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 7775 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 403:
#line 2715 "parse.y" /* yacc.c:1646  */
    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7783 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 406:
#line 2723 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7791 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 408:
#line 2730 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7799 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 416:
#line 2745 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7807 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 417:
#line 2749 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 7815 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 419:
#line 2756 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7823 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 420:
#line 2762 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7831 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 421:
#line 2766 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 7840 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 422:
#line 2772 "parse.y" /* yacc.c:1646  */
    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7849 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 423:
#line 2777 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 7857 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 424:
#line 2781 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 7865 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 425:
#line 2787 "parse.y" /* yacc.c:1646  */
    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7873 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 426:
#line 2791 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 7881 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 427:
#line 2797 "parse.y" /* yacc.c:1646  */
    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7889 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 428:
#line 2801 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7897 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 432:
#line 2814 "parse.y" /* yacc.c:1646  */
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 7907 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 433:
#line 2820 "parse.y" /* yacc.c:1646  */
    {
                      heredoc_end(p);
                    }
#line 7915 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 436:
#line 2830 "parse.y" /* yacc.c:1646  */
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 7925 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 437:
#line 2836 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 7934 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 438:
#line 2842 "parse.y" /* yacc.c:1646  */
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 7944 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 439:
#line 2850 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 7952 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 440:
#line 2854 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 7960 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 441:
#line 2861 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 7969 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 442:
#line 2866 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd))));
                    }
#line 7978 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 443:
#line 2873 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 7986 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 448:
#line 2883 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 7994 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 449:
#line 2887 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8002 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 450:
#line 2893 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8010 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 451:
#line 2897 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8018 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 454:
#line 2905 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8026 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 455:
#line 2909 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8034 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 456:
#line 2915 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 8042 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 457:
#line 2919 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 8050 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 458:
#line 2923 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 8058 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 459:
#line 2927 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 8066 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 460:
#line 2931 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 8074 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 461:
#line 2937 "parse.y" /* yacc.c:1646  */
    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8082 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 462:
#line 2943 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 8090 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 463:
#line 2947 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_nil(p);
                    }
#line 8098 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 464:
#line 2951 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_self(p);
                    }
#line 8106 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 465:
#line 2955 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_true(p);
                    }
#line 8114 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 466:
#line 2959 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_false(p);
                    }
#line 8122 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 467:
#line 2963 "parse.y" /* yacc.c:1646  */
    {
                      const char *fn = p->filename;
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 8134 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 468:
#line 2971 "parse.y" /* yacc.c:1646  */
    {
                      char buf[16];

                      snprintf(buf, sizeof(buf), "%d", p->lineno);
                      (yyval.nd) = new_int(p, buf, 10);
                    }
#line 8145 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 469:
#line 2978 "parse.y" /* yacc.c:1646  */
    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 8158 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 472:
#line 2993 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 8166 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 473:
#line 2997 "parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8175 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 474:
#line 3002 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8183 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 475:
#line 3013 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8193 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 476:
#line 3019 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8201 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 477:
#line 3025 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8209 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 478:
#line 3029 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8217 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 479:
#line 3033 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 8225 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 480:
#line 3037 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8233 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 481:
#line 3041 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8241 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 482:
#line 3045 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8249 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 483:
#line 3049 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].id));
                    }
#line 8257 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 484:
#line 3053 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8265 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 485:
#line 3057 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8273 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 486:
#line 3061 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 8281 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 487:
#line 3065 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8289 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 488:
#line 3069 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8297 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 489:
#line 3073 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8305 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 490:
#line 3077 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].id));
                    }
#line 8313 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 491:
#line 3081 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 8322 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 492:
#line 3088 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 8331 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 493:
#line 3093 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 8340 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 494:
#line 3098 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 8349 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 495:
#line 3103 "parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 8358 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 496:
#line 3110 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = 0;
                    }
#line 8366 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 497:
#line 3114 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8375 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 498:
#line 3121 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 8383 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 499:
#line 3125 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), 0);
                    }
#line 8391 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 500:
#line 3131 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8399 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 501:
#line 3135 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8407 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 502:
#line 3141 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[-1].id));
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 8416 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 503:
#line 3148 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), (yyvsp[0].nd));
                    }
#line 8425 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 504:
#line 3155 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), (yyvsp[0].nd));
                    }
#line 8434 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 505:
#line 3162 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8442 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 506:
#line 3166 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8450 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 507:
#line 3172 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8458 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 508:
#line 3176 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8466 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 511:
#line 3186 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8475 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 512:
#line 3191 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, 0);
                      (yyval.id) = -1;
                    }
#line 8484 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 515:
#line 3202 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8493 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 516:
#line 3209 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8501 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 517:
#line 3213 "parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, 0);
                      (yyval.id) = 0;
                    }
#line 8510 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 518:
#line 3220 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8519 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 519:
#line 3224 "parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_BEG;}
#line 8525 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 520:
#line 3225 "parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[-1].nd) == 0) {
                        yyerror(p, "can't define singleton method for ().");
                      }
                      else {
                        switch ((enum node_type)intn((yyvsp[-1].nd)->car)) {
                        case NODE_STR:
                        case NODE_DSTR:
                        case NODE_XSTR:
                        case NODE_DXSTR:
                        case NODE_DREGX:
                        case NODE_MATCH:
                        case NODE_FLOAT:
                        case NODE_ARRAY:
                        case NODE_HEREDOC:
                          yyerror(p, "can't define singleton method for literals");
                        default:
                          break;
                        }
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8552 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 522:
#line 3251 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8560 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 523:
#line 3257 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 8569 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 524:
#line 3262 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8577 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 525:
#line 3268 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8587 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 526:
#line 3274 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8596 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 527:
#line 3279 "parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if ((yyvsp[-2].nd)->car == (node*)NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 8610 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 540:
#line 3311 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = '.';
                    }
#line 8618 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 541:
#line 3315 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = 0;
                    }
#line 8626 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 543:
#line 3322 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = tCOLON2;
                    }
#line 8634 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 553:
#line 3346 "parse.y" /* yacc.c:1646  */
    {yyerrok;}
#line 8640 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 556:
#line 3352 "parse.y" /* yacc.c:1646  */
    {
                      p->lineno++;
                      p->column = 0;
                    }
#line 8649 "parse.tab.c" /* yacc.c:1646  */
    break;

  case 559:
#line 3363 "parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 8657 "parse.tab.c" /* yacc.c:1646  */
    break;


#line 8661 "parse.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (p, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (p, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, p);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (p, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, p);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 3367 "parse.y" /* yacc.c:1906  */

#define pylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nerr].message = c;
    p->error_buffer[p->nerr].lineno = p->lineno;
    p->error_buffer[p->nerr].column = p->column;
  }
  p->nerr++;
}

static void
yyerror_i(parser_state *p, const char *fmt, int i)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, i);
  yyerror(p, buf);
}

static void
yywarn(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nwarn < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->warn_buffer[p->nwarn].message = c;
    p->warn_buffer[p->nwarn].lineno = p->lineno;
    p->warn_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning(parser_state *p, const char *s)
{
  yywarn(p, s);
}

static void
yywarning_s(parser_state *p, const char *fmt, const char *s)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, s);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  int c;

  c = (int)(intptr_t)n->car;

  if (c == NODE_NTH_REF) {
    yyerror_i(p, "can't set variable $%" MRB_PRId, (int)(intptr_t)n->cdr);
  }
  else if (c == NODE_BACK_REF) {
    yyerror_i(p, "can't set variable $%c", (int)(intptr_t)n->cdr);
  }
  else {
    mrb_bug(p->mrb, "Internal error in backref_error() : n=>car == %S", mrb_fixnum_value(c));
  }
}

static void
void_expr_error(parser_state *p, node *n)
{
  int c;

  if (n == NULL) return;
  c = (int)(intptr_t)n->car;
  switch (c) {
  case NODE_BREAK:
  case NODE_RETURN:
  case NODE_NEXT:
  case NODE_REDO:
  case NODE_RETRY:
    yyerror(p, "void value expression");
    break;
  case NODE_AND:
  case NODE_OR:
    void_expr_error(p, n->cdr->car);
    void_expr_error(p, n->cdr->cdr);
    break;
  case NODE_BEGIN:
    if (n->cdr) {
      while (n->cdr) {
        n = n->cdr;
      }
      void_expr_error(p, n->car);
    }
    break;
  default:
    break;
  }
}

static void pushback(parser_state *p, int c);
static mrb_bool peeks(parser_state *p, const char *s);
static mrb_bool skips(parser_state *p, const char *s);

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = (int)(intptr_t)p->pb->car;
    tmp = p->pb;
    p->pb = p->pb->cdr;
    cons_free(tmp);
  }
  else {
#ifndef MRB_DISABLE_STDIO
    if (p->f) {
      if (feof(p->f)) goto eof;
      c = fgetc(p->f);
      if (c == EOF) goto eof;
    }
    else
#endif
      if (!p->s || p->s >= p->send) {
        goto eof;
      }
      else {
        c = (unsigned char)*p->s++;
      }
  }
  if (c >= 0) {
    p->column++;
  }
  if (c == '\r') {
    c = nextc(p);
    if (c != '\n') {
      pushback(p, c);
      return '\r';
    }
    return c;
  }
  return c;

  eof:
  if (!p->cxt) return -1;
  else {
    if (p->cxt->partial_hook(p) < 0)
      return -1;                /* end of program(s) */
    return -2;                  /* end of a file in the program files */
  }
}

static void
pushback(parser_state *p, int c)
{
  if (c >= 0) {
    p->column--;
  }
  p->pb = cons((node*)(intptr_t)c, p->pb);
}

static void
skip(parser_state *p, char term)
{
  int c;

  for (;;) {
    c = nextc(p);
    if (c < 0) break;
    if (c == term) break;
  }
}

static int
peekc_n(parser_state *p, int n)
{
  node *list = 0;
  int c0;

  do {
    c0 = nextc(p);
    if (c0 == -1) return c0;    /* do not skip partial EOF */
    if (c0 >= 0) --p->column;
    list = push(list, (node*)(intptr_t)c0);
  } while(n--);
  if (p->pb) {
    p->pb = append((node*)list, p->pb);
  }
  else {
    p->pb = list;
  }
  return c0;
}

static mrb_bool
peek_n(parser_state *p, int c, int n)
{
  return peekc_n(p, n) == c && c >= 0;
}
#define peek(p,c) peek_n((p), (c), 0)

static mrb_bool
peeks(parser_state *p, const char *s)
{
  size_t len = strlen(s);

#ifndef MRB_DISABLE_STDIO
  if (p->f) {
    int n = 0;
    while (*s) {
      if (!peek_n(p, *s++, n++)) return FALSE;
    }
    return TRUE;
  }
  else
#endif
    if (p->s && p->s + len <= p->send) {
      if (memcmp(p->s, s, len) == 0) return TRUE;
    }
  return FALSE;
}

static mrb_bool
skips(parser_state *p, const char *s)
{
  int c;

  for (;;) {
    /* skip until first char */
    for (;;) {
      c = nextc(p);
      if (c < 0) return FALSE;
      if (c == '\n') {
        p->lineno++;
        p->column = 0;
      }
      if (c == *s) break;
    }
    s++;
    if (peeks(p, s)) {
      size_t len = strlen(s);

      while (len--) {
        if (nextc(p) == '\n') {
          p->lineno++;
          p->column = 0;
        }
      }
      return TRUE;
    }
    else{
      s--;
    }
  }
  return FALSE;
}


static int
newtok(parser_state *p)
{
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
    p->tokbuf = p->buf;
    p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  }
  p->tidx = 0;
  return p->column - 1;
}

static void
tokadd(parser_state *p, int32_t c)
{
  char utf8[4];
  int i, len;

  /* mrb_assert(-0x10FFFF <= c && c <= 0xFF); */
  if (c >= 0) {
    /* Single byte from source or non-Unicode escape */
    utf8[0] = (char)c;
    len = 1;
  }
  else {
    /* Unicode character */
    c = -c;
    if (c < 0x80) {
      utf8[0] = (char)c;
      len = 1;
    }
    else if (c < 0x800) {
      utf8[0] = (char)(0xC0 | (c >> 6));
      utf8[1] = (char)(0x80 | (c & 0x3F));
      len = 2;
    }
    else if (c < 0x10000) {
      utf8[0] = (char)(0xE0 |  (c >> 12)        );
      utf8[1] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[2] = (char)(0x80 | ( c        & 0x3F));
      len = 3;
    }
    else {
      utf8[0] = (char)(0xF0 |  (c >> 18)        );
      utf8[1] = (char)(0x80 | ((c >> 12) & 0x3F));
      utf8[2] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[3] = (char)(0x80 | ( c        & 0x3F));
      len = 4;
    }
  }
  if (p->tidx+len >= p->tsiz) {
    if (p->tsiz >= MRB_PARSER_TOKBUF_MAX) {
      p->tidx += len;
      return;
    }
    p->tsiz *= 2;
    if (p->tokbuf == p->buf) {
      p->tokbuf = (char*)mrb_malloc(p->mrb, p->tsiz);
      memcpy(p->tokbuf, p->buf, MRB_PARSER_TOKBUF_SIZE);
    }
    else {
      p->tokbuf = (char*)mrb_realloc(p->mrb, p->tokbuf, p->tsiz);
    }
  }
  for (i = 0; i < len; i++) {
    p->tokbuf[p->tidx++] = utf8[i];
  }
}

static int
toklast(parser_state *p)
{
  return p->tokbuf[p->tidx-1];
}

static void
tokfix(parser_state *p)
{
  if (p->tidx >= MRB_PARSER_TOKBUF_MAX) {
    p->tidx = MRB_PARSER_TOKBUF_MAX-1;
    yyerror(p, "string too long (truncated)");
  }
  p->tokbuf[p->tidx] = '\0';
}

static const char*
tok(parser_state *p)
{
  return p->tokbuf;
}

static int
toklen(parser_state *p)
{
  return p->tidx;
}

#define IS_ARG() (p->lstate == EXPR_ARG || p->lstate == EXPR_CMDARG)
#define IS_END() (p->lstate == EXPR_END || p->lstate == EXPR_ENDARG || p->lstate == EXPR_ENDFN)
#define IS_BEG() (p->lstate == EXPR_BEG || p->lstate == EXPR_MID || p->lstate == EXPR_VALUE || p->lstate == EXPR_CLASS)
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))

static int32_t
scan_oct(const int *start, int len, int *retlen)
{
  const int *s = start;
  int32_t retval = 0;

  /* mrb_assert(len <= 3) */
  while (len-- && *s >= '0' && *s <= '7') {
    retval <<= 3;
    retval |= *s++ - '0';
  }
  *retlen = (int)(s - start);

  return retval;
}

static int32_t
scan_hex(parser_state *p, const int *start, int len, int *retlen)
{
  static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
  const int *s = start;
  uint32_t retval = 0;
  char *tmp;

  /* mrb_assert(len <= 8) */
  while (len-- && *s && (tmp = (char*)strchr(hexdigit, *s))) {
    retval <<= 4;
    retval |= (tmp - hexdigit) & 15;
    s++;
  }
  *retlen = (int)(s - start);

  return (int32_t)retval;
}

static int32_t
read_escape_unicode(parser_state *p, int limit)
{
  int buf[9];
  int i;
  int32_t hex;

  /* Look for opening brace */
  i = 0;
  buf[0] = nextc(p);
  if (buf[0] < 0) {
  eof:
    yyerror(p, "invalid escape character syntax");
    return -1;
  }
  if (ISXDIGIT(buf[0])) {
    /* \uxxxx form */
    for (i=1; i<limit; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
  }
  else {
    pushback(p, buf[0]);
  }
  hex = scan_hex(p, buf, i, &i);
  if (i == 0 || hex > 0x10FFFF || (hex & 0xFFFFF800) == 0xD800) {
    yyerror(p, "invalid Unicode code point");
    return -1;
  }
  return hex;
}

/* Return negative to indicate Unicode code point */
static int32_t
read_escape(parser_state *p)
{
  int32_t c;

  switch (c = nextc(p)) {
  case '\\':/* Backslash */
    return c;

  case 'n':/* newline */
    return '\n';

  case 't':/* horizontal tab */
    return '\t';

  case 'r':/* carriage-return */
    return '\r';

  case 'f':/* form-feed */
    return '\f';

  case 'v':/* vertical tab */
    return '\13';

  case 'a':/* alarm(bell) */
    return '\007';

  case 'e':/* escape */
    return 033;

  case '0': case '1': case '2': case '3': /* octal constant */
  case '4': case '5': case '6': case '7':
  {
    int buf[3];
    int i;

    buf[0] = c;
    for (i=1; i<3; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (buf[i] < '0' || '7' < buf[i]) {
        pushback(p, buf[i]);
        break;
      }
    }
    c = scan_oct(buf, i, &i);
  }
  return c;

  case 'x':     /* hex constant */
  {
    int buf[2];
    int i;

    for (i=0; i<2; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
    if (i == 0) {
      yyerror(p, "invalid hex escape");
      return -1;
    }
    return scan_hex(p, buf, i, &i);
  }

  case 'u':     /* Unicode */
    if (peek(p, '{')) {
      /* \u{xxxxxxxx} form */
      nextc(p);
      c = read_escape_unicode(p, 8);
      if (c < 0) return 0;
      if (nextc(p) != '}') goto eof;
    }
    else {
      c = read_escape_unicode(p, 4);
      if (c < 0) return 0;
    }
  return -c;

  case 'b':/* backspace */
    return '\010';

  case 's':/* space */
    return ' ';

  case 'M':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
    if ((c = nextc(p)) == '\\') {
      return read_escape(p) | 0x80;
    }
    else if (c < 0) goto eof;
    else {
      return ((c & 0xff) | 0x80);
    }

  case 'C':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
  case 'c':
    if ((c = nextc(p))== '\\') {
      c = read_escape(p);
    }
    else if (c == '?')
      return 0177;
    else if (c < 0) goto eof;
    return c & 0x9f;

    eof:
  case -1:
  case -2:                      /* end of a file */
    yyerror(p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)(intptr_t)p->lex_strterm->car;
  int nest_level = intn(p->lex_strterm->cdr->car);
  int beg = intn(p->lex_strterm->cdr->cdr->car);
  int end = intn(p->lex_strterm->cdr->cdr->cdr);
  parser_heredoc_info *hinf = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_inf(p) : NULL;

  if (beg == 0) beg = -3;       /* should never happen */
  if (end == 0) end = -3;
  newtok(p);
  while ((c = nextc(p)) != end || nest_level != 0) {
    if (hinf && (c == '\n' || c < 0)) {
      mrb_bool line_head;
      tokadd(p, '\n');
      tokfix(p);
      p->lineno++;
      p->column = 0;
      line_head = hinf->line_head;
      hinf->line_head = TRUE;
      if (line_head) {
        /* check whether end of heredoc */
        const char *s = tok(p);
        int len = toklen(p);
        if (hinf->allow_indent) {
          while (ISSPACE(*s) && len > 0) {
            ++s;
            --len;
          }
        }
        if ((len-1 == hinf->term_len) && (strncmp(s, hinf->term, len-1) == 0)) {
          if (c < 0) {
            p->parsing_heredoc = NULL;
          }
          else {
            return tHEREDOC_END;
          }
        }
      }
      if (c < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "can't find heredoc delimiter \"%s\" anywhere before EOF", hinf->term);
        yyerror(p, buf);
        return 0;
      }
      pylval.nd = new_str(p, tok(p), toklen(p));
      return tHD_STRING_MID;
    }
    if (c < 0) {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == beg) {
      nest_level++;
      p->lex_strterm->cdr->car = (node*)(intptr_t)nest_level;
    }
    else if (c == end) {
      nest_level--;
      p->lex_strterm->cdr->car = (node*)(intptr_t)nest_level;
    }
    else if (c == '\\') {
      c = nextc(p);
      if (type & STR_FUNC_EXPAND) {
        if (c == end || c == beg) {
          tokadd(p, c);
        }
        else if (c == '\n') {
          p->lineno++;
          p->column = 0;
          if (type & STR_FUNC_ARRAY) {
            tokadd(p, '\n');
          }
        }
        else if (type & STR_FUNC_REGEXP) {
          tokadd(p, '\\');
          tokadd(p, c);
        }
        else if (c == 'u' && peek(p, '{')) {
          /* \u{xxxx xxxx xxxx} form */
          nextc(p);
          while (1) {
            do c = nextc(p); while (ISSPACE(c));
            if (c == '}') break;
            pushback(p, c);
            c = read_escape_unicode(p, 8);
            if (c < 0) break;
            tokadd(p, -c);
          }
          if (hinf)
            hinf->line_head = FALSE;
        }
        else {
          pushback(p, c);
          tokadd(p, read_escape(p));
          if (hinf)
            hinf->line_head = FALSE;
        }
      }
      else {
        if (c != beg && c != end) {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
          }
          if (!(c == '\\' || ((type & STR_FUNC_ARRAY) && ISSPACE(c)))) {
            tokadd(p, '\\');
          }
        }
        tokadd(p, c);
      }
      continue;
    }
    else if ((c == '#') && (type & STR_FUNC_EXPAND)) {
      c = nextc(p);
      if (c == '{') {
        tokfix(p);
        p->lstate = EXPR_BEG;
        p->cmd_start = TRUE;
        pylval.nd = new_str(p, tok(p), toklen(p));
        if (hinf) {
          hinf->line_head = FALSE;
          return tHD_STRING_PART;
        }
        return tSTRING_PART;
      }
      tokadd(p, '#');
      pushback(p, c);
      continue;
    }
    if ((type & STR_FUNC_ARRAY) && ISSPACE(c)) {
      if (toklen(p) == 0) {
        do {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
            heredoc_treat_nextline(p);
            if (p->parsing_heredoc != NULL) {
              return tHD_LITERAL_DELIM;
            }
          }
          c = nextc(p);
        } while (ISSPACE(c));
        pushback(p, c);
        return tLITERAL_DELIM;
      }
      else {
        pushback(p, c);
        tokfix(p);
        pylval.nd = new_str(p, tok(p), toklen(p));
        return tSTRING_MID;
      }
    }
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
    }
    tokadd(p, c);
  }

  tokfix(p);
  p->lstate = EXPR_ENDARG;
  end_strterm(p);

  if (type & STR_FUNC_XQUOTE) {
    pylval.nd = new_xstr(p, tok(p), toklen(p));
    return tXSTRING;
  }

  if (type & STR_FUNC_REGEXP) {
    int f = 0;
    int re_opt;
    char *s = strndup(tok(p), toklen(p));
    char flags[3];
    char *flag = flags;
    char enc = '\0';
    char *encp;
    char *dup;

    newtok(p);
    while (re_opt = nextc(p), re_opt >= 0 && ISALPHA(re_opt)) {
      switch (re_opt) {
      case 'i': f |= 1; break;
      case 'x': f |= 2; break;
      case 'm': f |= 4; break;
      case 'u': f |= 16; break;
      case 'n': f |= 32; break;
      default: tokadd(p, re_opt); break;
      }
    }
    pushback(p, re_opt);
    if (toklen(p)) {
      char msg[128];
      tokfix(p);
      snprintf(msg, sizeof(msg), "unknown regexp option%s - %s",
          toklen(p) > 1 ? "s" : "", tok(p));
      yyerror(p, msg);
    }
    if (f != 0) {
      if (f & 1) *flag++ = 'i';
      if (f & 2) *flag++ = 'x';
      if (f & 4) *flag++ = 'm';
      if (f & 16) enc = 'u';
      if (f & 32) enc = 'n';
    }
    if (flag > flags) {
      dup = strndup(flags, (size_t)(flag - flags));
    }
    else {
      dup = NULL;
    }
    if (enc) {
      encp = strndup(&enc, 1);
    }
    else {
      encp = NULL;
    }
    pylval.nd = new_regx(p, s, dup, encp);

    return tREGEXP;
  }
  pylval.nd = new_str(p, tok(p), toklen(p));

  return tSTRING;
}


static int
heredoc_identifier(parser_state *p)
{
  int c;
  int type = str_heredoc;
  mrb_bool indent = FALSE;
  mrb_bool quote = FALSE;
  node *newnode;
  parser_heredoc_info *info;

  c = nextc(p);
  if (ISSPACE(c) || c == '=') {
    pushback(p, c);
    return 0;
  }
  if (c == '-') {
    indent = TRUE;
    c = nextc(p);
  }
  if (c == '\'' || c == '"') {
    int term = c;
    if (c == '\'')
      quote = TRUE;
    newtok(p);
    while ((c = nextc(p)) >= 0 && c != term) {
      if (c == '\n') {
        c = -1;
        break;
      }
      tokadd(p, c);
    }
    if (c < 0) {
      yyerror(p, "unterminated here document identifier");
      return 0;
    }
  }
  else {
    if (c < 0) {
      return 0;                 /* missing here document identifier */
    }
    if (! identchar(c)) {
      pushback(p, c);
      if (indent) pushback(p, '-');
      return 0;
    }
    newtok(p);
    do {
      tokadd(p, c);
    } while ((c = nextc(p)) >= 0 && identchar(c));
    pushback(p, c);
  }
  tokfix(p);
  newnode = new_heredoc(p);
  info = (parser_heredoc_info*)newnode->cdr;
  info->term = strndup(tok(p), toklen(p));
  info->term_len = toklen(p);
  if (! quote)
    type |= STR_FUNC_EXPAND;
  info->type = (string_type)type;
  info->allow_indent = indent;
  info->line_head = TRUE;
  info->doc = NULL;
  p->heredocs_from_nextline = push(p->heredocs_from_nextline, newnode);
  p->lstate = EXPR_END;

  pylval.nd = newnode;
  return tHEREDOC_BEG;
}

static int
arg_ambiguous(parser_state *p)
{
  yywarning(p, "ambiguous first argument; put parentheses or even spaces");
  return 1;
}

#include "lex.def"

static int
parser_yylex(parser_state *p)
{
  int32_t c;
  int space_seen = 0;
  int cmd_state;
  enum mrb_lex_state_enum last_state;
  int token_column;

  if (p->lex_strterm) {
    if (is_strterm_type(p, STR_FUNC_HEREDOC)) {
      if (p->parsing_heredoc != NULL)
        return parse_string(p);
    }
    else
      return parse_string(p);
  }
  cmd_state = p->cmd_start;
  p->cmd_start = FALSE;
  retry:
  last_state = p->lstate;
  switch (c = nextc(p)) {
  case '\004':  /* ^D */
  case '\032':  /* ^Z */
  case '\0':    /* NUL */
  case -1:      /* end of script. */
    if (p->heredocs_from_nextline)
      goto maybe_heredoc;
    return 0;

  /* white spaces */
  case ' ': case '\t': case '\f': case '\r':
  case '\13':   /* '\v' */
    space_seen = 1;
    goto retry;

  case '#':     /* it's a comment */
    skip(p, '\n');
    /* fall through */
  case -2:      /* end of a file */
  case '\n':
    maybe_heredoc:
    heredoc_treat_nextline(p);
  switch (p->lstate) {
  case EXPR_BEG:
  case EXPR_FNAME:
  case EXPR_DOT:
  case EXPR_CLASS:
  case EXPR_VALUE:
    p->lineno++;
    p->column = 0;
    if (p->parsing_heredoc != NULL) {
      if (p->lex_strterm) {
        return parse_string(p);
      }
    }
    goto retry;
  default:
    break;
  }
  if (p->parsing_heredoc != NULL) {
    return '\n';
  }
  while ((c = nextc(p))) {
    switch (c) {
    case ' ': case '\t': case '\f': case '\r':
    case '\13': /* '\v' */
      space_seen = 1;
      break;
    case '.':
      if ((c = nextc(p)) != '.') {
        pushback(p, c);
        pushback(p, '.');
        goto retry;
      }
    case -1:                  /* EOF */
    case -2:                  /* end of a file */
      goto normal_newline;
    default:
      pushback(p, c);
      goto normal_newline;
    }
  }
  normal_newline:
  p->cmd_start = TRUE;
  p->lstate = EXPR_BEG;
  return '\n';

  case '*':
    if ((c = nextc(p)) == '*') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("**",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      c = tPOW;
    }
    else {
      if (c == '=') {
        pylval.id = intern_c('*');
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "'*' interpreted as argument prefix");
        c = tSTAR;
      }
      else if (IS_BEG()) {
        c = tSTAR;
      }
      else {
        c = '*';
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '!':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return '!';
      }
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if (c == '=') {
      return tNEQ;
    }
    if (c == '~') {
      return tNMATCH;
    }
    pushback(p, c);
    return '!';

  case '=':
    if (p->column == 1) {
      static const char begin[] = "begin";
      static const char end[] = "\n=end";
      if (peeks(p, begin)) {
        c = peekc_n(p, sizeof(begin)-1);
        if (c < 0 || ISSPACE(c)) {
          do {
            if (!skips(p, end)) {
              yyerror(p, "embedded document meets end of file");
              return 0;
            }
            c = nextc(p);
          } while (!(c < 0 || ISSPACE(c)));
          if (c != '\n') skip(p, '\n');
          p->lineno++;
          p->column = 0;
          goto retry;
        }
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      if ((c = nextc(p)) == '=') {
        return tEQQ;
      }
      pushback(p, c);
      return tEQ;
    }
    if (c == '~') {
      return tMATCH;
    }
    else if (c == '>') {
      return tASSOC;
    }
    pushback(p, c);
    return '=';

  case '<':
    c = nextc(p);
    if (c == '<' &&
        p->lstate != EXPR_DOT &&
        p->lstate != EXPR_CLASS &&
        !IS_END() &&
        (!IS_ARG() || space_seen)) {
      int token = heredoc_identifier(p);
      if (token)
        return token;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
      if (p->lstate == EXPR_CLASS) {
        p->cmd_start = TRUE;
      }
    }
    if (c == '=') {
      if ((c = nextc(p)) == '>') {
        return tCMP;
      }
      pushback(p, c);
      return tLEQ;
    }
    if (c == '<') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("<<",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tLSHFT;
    }
    pushback(p, c);
    return '<';

  case '>':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      return tGEQ;
    }
    if (c == '>') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern(">>",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tRSHFT;
    }
    pushback(p, c);
    return '>';

  case '"':
    p->lex_strterm = new_strterm(p, str_dquote, '"', 0);
    return tSTRING_BEG;

  case '\'':
    p->lex_strterm = new_strterm(p, str_squote, '\'', 0);
    return parse_string(p);

  case '`':
    if (p->lstate == EXPR_FNAME) {
      p->lstate = EXPR_ENDFN;
      return '`';
    }
    if (p->lstate == EXPR_DOT) {
      if (cmd_state)
        p->lstate = EXPR_CMDARG;
      else
        p->lstate = EXPR_ARG;
      return '`';
    }
    p->lex_strterm = new_strterm(p, str_xquote, '`', 0);
    return tXSTRING_BEG;

  case '?':
    if (IS_END()) {
      p->lstate = EXPR_VALUE;
      return '?';
    }
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete character syntax");
      return 0;
    }
    if (ISSPACE(c)) {
      if (!IS_ARG()) {
        int c2;
        switch (c) {
        case ' ':
          c2 = 's';
          break;
        case '\n':
          c2 = 'n';
          break;
        case '\t':
          c2 = 't';
          break;
        case '\v':
          c2 = 'v';
          break;
        case '\r':
          c2 = 'r';
          break;
        case '\f':
          c2 = 'f';
          break;
        default:
          c2 = 0;
          break;
        }
        if (c2) {
          char buf[256];
          snprintf(buf, sizeof(buf), "invalid character syntax; use ?\\%c", c2);
          yyerror(p, buf);
        }
      }
      ternary:
      pushback(p, c);
      p->lstate = EXPR_VALUE;
      return '?';
    }
    newtok(p);
    /* need support UTF-8 if configured */
    if ((isalnum(c) || c == '_')) {
      int c2 = nextc(p);
      pushback(p, c2);
      if ((isalnum(c2) || c2 == '_')) {
        goto ternary;
      }
    }
    if (c == '\\') {
      c = read_escape(p);
      tokadd(p, c);
    }
    else {
      tokadd(p, c);
    }
    tokfix(p);
    pylval.nd = new_str(p, tok(p), toklen(p));
    p->lstate = EXPR_ENDARG;
    return tCHAR;

  case '&':
    if ((c = nextc(p)) == '&') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("&&",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tANDOP;
    }
    else if (c == '.') {
      p->lstate = EXPR_DOT;
      return tANDDOT;
    }
    else if (c == '=') {
      pylval.id = intern_c('&');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      yywarning(p, "'&' interpreted as argument prefix");
      c = tAMPER;
    }
    else if (IS_BEG()) {
      c = tAMPER;
    }
    else {
      c = '&';
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '|':
    if ((c = nextc(p)) == '|') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("||",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      pylval.id = intern_c('|');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '|';

  case '+':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUPLUS;
      }
      pushback(p, c);
      return '+';
    }
    if (c == '=') {
      pylval.id = intern_c('+');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        c = '+';
        goto start_num;
      }
      return tUPLUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '+';

  case '-':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUMINUS;
      }
      pushback(p, c);
      return '-';
    }
    if (c == '=') {
      pylval.id = intern_c('-');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (c == '>') {
      p->lstate = EXPR_ENDFN;
      return tLAMBDA;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        return tUMINUS_NUM;
      }
      return tUMINUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '-';

  case '.':
    p->lstate = EXPR_BEG;
    if ((c = nextc(p)) == '.') {
      if ((c = nextc(p)) == '.') {
        return tDOT3;
      }
      pushback(p, c);
      return tDOT2;
    }
    pushback(p, c);
    if (c >= 0 && ISDIGIT(c)) {
      yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
    }
    p->lstate = EXPR_DOT;
    return '.';

    start_num:
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
  {
    int is_float, seen_point, seen_e, nondigit;

    is_float = seen_point = seen_e = nondigit = 0;
    p->lstate = EXPR_ENDARG;
    newtok(p);
    if (c == '-' || c == '+') {
      tokadd(p, c);
      c = nextc(p);
    }
    if (c == '0') {
#define no_digits() do {yyerror(p,"numeric literal without digits"); return 0;} while (0)
      int start = toklen(p);
      c = nextc(p);
      if (c == 'x' || c == 'X') {
        /* hexadecimal */
        c = nextc(p);
        if (c >= 0 && ISXDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISXDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, tolower(c));
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 16);
        return tINTEGER;
      }
      if (c == 'b' || c == 'B') {
        /* binary */
        c = nextc(p);
        if (c == '0' || c == '1') {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (c != '0' && c != '1') break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 2);
        return tINTEGER;
      }
      if (c == 'd' || c == 'D') {
        /* decimal */
        c = nextc(p);
        if (c >= 0 && ISDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 10);
        return tINTEGER;
      }
      if (c == '_') {
        /* 0_0 */
        goto octal_number;
      }
      if (c == 'o' || c == 'O') {
        /* prefixed octal */
        c = nextc(p);
        if (c < 0 || c == '_' || !ISDIGIT(c)) {
          no_digits();
        }
      }
      if (c >= '0' && c <= '7') {
        /* octal */
        octal_number:
        do {
          if (c == '_') {
            if (nondigit) break;
            nondigit = c;
            continue;
          }
          if (c < '0' || c > '9') break;
          if (c > '7') goto invalid_octal;
          nondigit = 0;
          tokadd(p, c);
        } while ((c = nextc(p)) >= 0);

        if (toklen(p) > start) {
          pushback(p, c);
          tokfix(p);
          if (nondigit) goto trailing_uc;
          pylval.nd = new_int(p, tok(p), 8);
          return tINTEGER;
        }
        if (nondigit) {
          pushback(p, c);
          goto trailing_uc;
        }
      }
      if (c > '7' && c <= '9') {
        invalid_octal:
        yyerror(p, "Invalid octal digit");
      }
      else if (c == '.' || c == 'e' || c == 'E') {
        tokadd(p, '0');
      }
      else {
        pushback(p, c);
        pylval.nd = new_int(p, "0", 10);
        return tINTEGER;
      }
    }

    for (;;) {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        nondigit = 0;
        tokadd(p, c);
        break;

      case '.':
        if (nondigit) goto trailing_uc;
        if (seen_point || seen_e) {
          goto decode_num;
        }
        else {
          int c0 = nextc(p);
          if (c0 < 0 || !ISDIGIT(c0)) {
            pushback(p, c0);
            goto decode_num;
          }
          c = c0;
        }
        tokadd(p, '.');
        tokadd(p, c);
        is_float++;
        seen_point++;
        nondigit = 0;
        break;

      case 'e':
      case 'E':
        if (nondigit) {
          pushback(p, c);
          c = nondigit;
          goto decode_num;
        }
        if (seen_e) {
          goto decode_num;
        }
        tokadd(p, c);
        seen_e++;
        is_float++;
        nondigit = c;
        c = nextc(p);
        if (c != '-' && c != '+') continue;
        tokadd(p, c);
        nondigit = c;
        break;

      case '_':       /* '_' in number just ignored */
        if (nondigit) goto decode_num;
        nondigit = c;
        break;

      default:
        goto decode_num;
      }
      c = nextc(p);
    }

    decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
      yyerror_i(p, "trailing '%c' in number", nondigit);
    }
    tokfix(p);
    if (is_float) {
#ifdef MRB_WITHOUT_FLOAT
      yywarning_s(p, "floating point numbers are not supported", tok(p));
      pylval.nd = new_int(p, "0", 10);
      return tINTEGER;
#else
      double d;
      char *endp;

      errno = 0;
      d = mrb_float_read(tok(p), &endp);
      if (d == 0 && endp == tok(p)) {
        yywarning_s(p, "corrupted float value %s", tok(p));
      }
      else if (errno == ERANGE) {
        yywarning_s(p, "float %s out of range", tok(p));
        errno = 0;
      }
      pylval.nd = new_float(p, tok(p));
      return tFLOAT;
#endif
    }
    pylval.nd = new_int(p, tok(p), 10);
    return tINTEGER;
  }

  case ')':
  case ']':
    p->paren_nest--;
    /* fall through */
  case '}':
    COND_LEXPOP();
    CMDARG_LEXPOP();
    if (c == ')')
      p->lstate = EXPR_ENDFN;
    else
      p->lstate = EXPR_END;
    return c;

  case ':':
    c = nextc(p);
    if (c == ':') {
      if (IS_BEG() || p->lstate == EXPR_CLASS || IS_SPCARG(-1)) {
        p->lstate = EXPR_BEG;
        return tCOLON3;
      }
      p->lstate = EXPR_DOT;
      return tCOLON2;
    }
    if (!space_seen && IS_END()) {
      pushback(p, c);
      p->lstate = EXPR_BEG;
      return tLABEL_TAG;
    }
    if (!ISSPACE(c) || IS_BEG()) {
      pushback(p, c);
      p->lstate = EXPR_FNAME;
      return tSYMBEG;
    }
    pushback(p, c);
    p->lstate = EXPR_BEG;
    return ':';

  case '/':
    if (IS_BEG()) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('/');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '/';

  case '^':
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('^');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '^';

  case ';':
    p->lstate = EXPR_BEG;
    return ';';

  case ',':
    p->lstate = EXPR_BEG;
    return ',';

  case '~':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      if ((c = nextc(p)) != '@') {
        pushback(p, c);
      }
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '~';

  case '(':
    if (IS_BEG()) {
      c = tLPAREN;
    }
    else if (IS_SPCARG(-1)) {
      c = tLPAREN_ARG;
    }
    else if (p->lstate == EXPR_END && space_seen) {
      c = tLPAREN_ARG;
    }
    p->paren_nest++;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '[':
    p->paren_nest++;
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if ((c = nextc(p)) == ']') {
        if ((c = nextc(p)) == '=') {
          return tASET;
        }
        pushback(p, c);
        return tAREF;
      }
      pushback(p, c);
      return '[';
    }
    else if (IS_BEG()) {
      c = tLBRACK;
    }
    else if (IS_ARG() && space_seen) {
      c = tLBRACK;
    }
    p->lstate = EXPR_BEG;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    return c;

  case '{':
    if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
      p->lstate = EXPR_BEG;
      p->lpar_beg = 0;
      p->paren_nest--;
      COND_PUSH(0);
      CMDARG_PUSH(0);
      return tLAMBEG;
    }
    if (IS_ARG() || p->lstate == EXPR_END || p->lstate == EXPR_ENDFN)
      c = '{';          /* block (primary) */
    else if (p->lstate == EXPR_ENDARG)
      c = tLBRACE_ARG;  /* block (expr) */
    else
      c = tLBRACE;      /* hash */
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '\\':
    c = nextc(p);
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
      space_seen = 1;
      goto retry; /* skip \\n */
    }
    pushback(p, c);
    return '\\';

  case '%':
    if (IS_BEG()) {
      int term;
      int paren;

      c = nextc(p);
      quotation:
      if (c < 0 || !ISALNUM(c)) {
        term = c;
        c = 'Q';
      }
      else {
        term = nextc(p);
        if (isalnum(term)) {
          yyerror(p, "unknown type of %string");
          return 0;
        }
      }
      if (c < 0 || term < 0) {
        yyerror(p, "unterminated quoted string meets end of file");
        return 0;
      }
      paren = term;
      if (term == '(') term = ')';
      else if (term == '[') term = ']';
      else if (term == '{') term = '}';
      else if (term == '<') term = '>';
      else paren = 0;

      switch (c) {
      case 'Q':
        p->lex_strterm = new_strterm(p, str_dquote, term, paren);
        return tSTRING_BEG;

      case 'q':
        p->lex_strterm = new_strterm(p, str_squote, term, paren);
        return parse_string(p);

      case 'W':
        p->lex_strterm = new_strterm(p, str_dword, term, paren);
        return tWORDS_BEG;

      case 'w':
        p->lex_strterm = new_strterm(p, str_sword, term, paren);
        return tWORDS_BEG;

      case 'x':
        p->lex_strterm = new_strterm(p, str_xquote, term, paren);
        return tXSTRING_BEG;

      case 'r':
        p->lex_strterm = new_strterm(p, str_regexp, term, paren);
        return tREGEXP_BEG;

      case 's':
        p->lex_strterm = new_strterm(p, str_ssym, term, paren);
        return tSYMBEG;

      case 'I':
        p->lex_strterm = new_strterm(p, str_dsymbols, term, paren);
        return tSYMBOLS_BEG;

      case 'i':
        p->lex_strterm = new_strterm(p, str_ssymbols, term, paren);
        return tSYMBOLS_BEG;

      default:
        yyerror(p, "unknown type of %string");
        return 0;
      }
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('%');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_SPCARG(c)) {
      goto quotation;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '%';

  case '$':
    p->lstate = EXPR_END;
    token_column = newtok(p);
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete global variable syntax");
      return 0;
    }
    switch (c) {
    case '_':     /* $_: last read line string */
      c = nextc(p);
      if (c >= 0 && identchar(c)) { /* if there is more after _ it is a variable */
        tokadd(p, '$');
        tokadd(p, c);
        break;
      }
      pushback(p, c);
      c = '_';
      /* fall through */
    case '~':     /* $~: match-data */
    case '*':     /* $*: argv */
    case '$':     /* $$: pid */
    case '?':     /* $?: last status */
    case '!':     /* $!: error string */
    case '@':     /* $@: error position */
    case '/':     /* $/: input record separator */
    case '\\':    /* $\: output record separator */
    case ';':     /* $;: field separator */
    case ',':     /* $,: output field separator */
    case '.':     /* $.: last read line number */
    case '=':     /* $=: ignorecase */
    case ':':     /* $:: load path */
    case '<':     /* $<: reading filename */
    case '>':     /* $>: default output handle */
    case '\"':    /* $": already loaded files */
      tokadd(p, '$');
      tokadd(p, c);
      tokfix(p);
      pylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '-':
      tokadd(p, '$');
      tokadd(p, c);
      c = nextc(p);
      pushback(p, c);
      gvar:
      tokfix(p);
      pylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '&':     /* $&: last match */
    case '`':     /* $`: string before last match */
    case '\'':    /* $': string after last match */
    case '+':     /* $+: string matches last pattern */
      if (last_state == EXPR_FNAME) {
        tokadd(p, '$');
        tokadd(p, c);
        goto gvar;
      }
      pylval.nd = new_back_ref(p, c);
      return tBACK_REF;

    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      do {
        tokadd(p, c);
        c = nextc(p);
      } while (c >= 0 && isdigit(c));
      pushback(p, c);
      if (last_state == EXPR_FNAME) goto gvar;
      tokfix(p);
      {
        unsigned long n = strtoul(tok(p), NULL, 10);
        if (n > INT_MAX) {
          yyerror_i(p, "capture group index must be <= %d", INT_MAX);
          return 0;
        }
        pylval.nd = new_nth_ref(p, (int)n);
      }
      return tNTH_REF;

    default:
      if (!identchar(c)) {
        pushback(p,  c);
        return '$';
      }
      /* fall through */
    case '0':
      tokadd(p, '$');
    }
    break;

    case '@':
      c = nextc(p);
      token_column = newtok(p);
      tokadd(p, '@');
      if (c == '@') {
        tokadd(p, '@');
        c = nextc(p);
      }
      if (c < 0) {
        if (p->tidx == 1) {
          yyerror(p, "incomplete instance variable syntax");
        }
        else {
          yyerror(p, "incomplete class variable syntax");
        }
        return 0;
      }
      else if (isdigit(c)) {
        if (p->tidx == 1) {
          yyerror_i(p, "'@%c' is not allowed as an instance variable name", c);
        }
        else {
          yyerror_i(p, "'@@%c' is not allowed as a class variable name", c);
        }
        return 0;
      }
      if (!identchar(c)) {
        pushback(p, c);
        return '@';
      }
      break;

    case '_':
      token_column = newtok(p);
      break;

    default:
      if (!identchar(c)) {
        yyerror_i(p,  "Invalid char '\\x%02X' in expression", c);
        goto retry;
      }

      token_column = newtok(p);
      break;
  }

  do {
    tokadd(p, c);
    c = nextc(p);
    if (c < 0) break;
  } while (identchar(c));
  if (token_column == 0 && toklen(p) == 7 && (c < 0 || c == '\n') &&
      strncmp(tok(p), "__END__", toklen(p)) == 0)
    return -1;

  switch (tok(p)[0]) {
  case '@': case '$':
    pushback(p, c);
    break;
  default:
    if ((c == '!' || c == '?') && !peek(p, '=')) {
      tokadd(p, c);
    }
    else {
      pushback(p, c);
    }
  }
  tokfix(p);
  {
    int result = 0;

    switch (tok(p)[0]) {
    case '$':
      p->lstate = EXPR_END;
      result = tGVAR;
      break;
    case '@':
      p->lstate = EXPR_END;
      if (tok(p)[1] == '@')
        result = tCVAR;
      else
        result = tIVAR;
      break;

    default:
      if (toklast(p) == '!' || toklast(p) == '?') {
        result = tFID;
      }
      else {
        if (p->lstate == EXPR_FNAME) {
          if ((c = nextc(p)) == '=' && !peek(p, '~') && !peek(p, '>') &&
              (!peek(p, '=') || (peek_n(p, '>', 1)))) {
            result = tIDENTIFIER;
            tokadd(p, c);
            tokfix(p);
          }
          else {
            pushback(p, c);
          }
        }
        if (result == 0 && ISUPPER(tok(p)[0])) {
          result = tCONSTANT;
        }
        else {
          result = tIDENTIFIER;
        }
      }

      if (IS_LABEL_POSSIBLE()) {
        if (IS_LABEL_SUFFIX(0)) {
          p->lstate = EXPR_END;
          tokfix(p);
          pylval.id = intern_cstr(tok(p));
          return tIDENTIFIER;
        }
      }
      if (p->lstate != EXPR_DOT) {
        const struct kwtable *kw;

        /* See if it is a reserved word.  */
        kw = mrb_reserved_word(tok(p), toklen(p));
        if (kw) {
          enum mrb_lex_state_enum state = p->lstate;
          pylval.num = p->lineno;
          p->lstate = kw->state;
          if (state == EXPR_FNAME) {
            pylval.id = intern_cstr(kw->name);
            return kw->id[0];
          }
          if (p->lstate == EXPR_BEG) {
            p->cmd_start = TRUE;
          }
          if (kw->id[0] == keyword_do) {
            if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
              p->lpar_beg = 0;
              p->paren_nest--;
              return keyword_do_LAMBDA;
            }
            if (COND_P()) return keyword_do_cond;
            if (CMDARG_P() && state != EXPR_CMDARG)
              return keyword_do_block;
            if (state == EXPR_ENDARG || state == EXPR_BEG)
              return keyword_do_block;
            return keyword_do;
          }
          if (state == EXPR_BEG || state == EXPR_VALUE)
            return kw->id[0];
          else {
            if (kw->id[0] != kw->id[1])
              p->lstate = EXPR_BEG;
            return kw->id[1];
          }
        }
      }

      if (IS_BEG() || p->lstate == EXPR_DOT || IS_ARG()) {
        if (cmd_state) {
          p->lstate = EXPR_CMDARG;
        }
        else {
          p->lstate = EXPR_ARG;
        }
      }
      else if (p->lstate == EXPR_FNAME) {
        p->lstate = EXPR_ENDFN;
      }
      else {
        p->lstate = EXPR_END;
      }
    }
    {
      mrb_sym ident = intern_cstr(tok(p));

      pylval.id = ident;
      if (last_state != EXPR_DOT && islower(tok(p)[0]) && local_var_p(p, ident)) {
        p->lstate = EXPR_END;
      }
    }
    return result;
  }
}

static int
yylex(void *lval, parser_state *p)
{
  p->ylval = lval;
  return parser_yylex(p);
}

static void
parser_init_cxt(parser_state *p, mrbc_context *cxt)
{
  if (!cxt) return;
  if (cxt->filename) mrb_parser_set_filename(p, cxt->filename);
  if (cxt->lineno) p->lineno = cxt->lineno;
  if (cxt->syms) {
    int i;

    p->locals = cons(0,0);
    for (i=0; i<cxt->slen; i++) {
      local_add_f(p, cxt->syms[i]);
    }
  }
  p->capture_errors = cxt->capture_errors;
  p->no_optimize = cxt->no_optimize;
  if (cxt->partial_hook) {
    p->cxt = cxt;
  }
}

static void
parser_update_cxt(parser_state *p, mrbc_context *cxt)
{
  node *n, *n0;
  int i = 0;

  if (!cxt) return;
  if ((int)(intptr_t)p->tree->car != NODE_SCOPE) return;
  n0 = n = p->tree->cdr->car;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym *)mrb_realloc(p->mrb, cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = sym(n->car);
  }
}

void mrb_codedump_all(mrb_state*, struct RProc*);
void mrb_parser_dump(mrb_state *mrb, node *tree, int offset);

MRB_API void
mrb_parser_parse(parser_state *p, mrbc_context *c)
{
  struct mrb_jmpbuf buf1;
  p->jmp = &buf1;

  MRB_TRY(p->jmp) {
    int n = 1;

    p->cmd_start = TRUE;
    p->in_def = p->in_single = 0;
    p->nerr = p->nwarn = 0;
    p->lex_strterm = NULL;

    parser_init_cxt(p, c);

    if (p->mrb->jmp) {
      n = yyparse(p);
    }
    else {
      struct mrb_jmpbuf buf2;

      p->mrb->jmp = &buf2;
      MRB_TRY(p->mrb->jmp) {
        n = yyparse(p);
      }
      MRB_CATCH(p->mrb->jmp) {
        p->nerr++;
      }
      MRB_END_EXC(p->mrb->jmp);
      p->mrb->jmp = 0;
    }
    if (n != 0 || p->nerr > 0) {
      p->tree = 0;
      return;
    }
    if (!p->tree) {
      p->tree = new_nil(p);
    }
    parser_update_cxt(p, c);
    if (c && c->dump_result) {
      mrb_parser_dump(p->mrb, p->tree, 0);
    }
  }
  MRB_CATCH(p->jmp) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = 0;
    return;
  }
  MRB_END_EXC(p->jmp);
}

MRB_API parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mrb_pool_open(mrb);
  if (!pool) return NULL;
  p = (parser_state *)mrb_pool_alloc(pool, sizeof(parser_state));
  if (!p) return NULL;

  *p = parser_state_zero;
  p->mrb = mrb;
  p->pool = pool;

  p->s = p->send = NULL;
#ifndef MRB_DISABLE_STDIO
  p->f = NULL;
#endif

  p->cmd_start = TRUE;
  p->in_def = p->in_single = 0;

  p->capture_errors = FALSE;
  p->lineno = 1;
  p->column = 0;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif
  p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  p->tokbuf = p->buf;

  p->lex_strterm = NULL;
  p->all_heredocs = p->parsing_heredoc = NULL;
  p->lex_strterm_before_heredoc = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

MRB_API void
mrb_parser_free(parser_state *p) {
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
  }
  mrb_pool_close(p->pool);
}

MRB_API mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  return (mrbc_context *)mrb_calloc(mrb, 1, sizeof(mrbc_context));
}

MRB_API void
mrbc_context_free(mrb_state *mrb, mrbc_context *cxt)
{
  mrb_free(mrb, cxt->filename);
  mrb_free(mrb, cxt->syms);
  mrb_free(mrb, cxt);
}

MRB_API const char*
mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s)
{
  if (s) {
    size_t len = strlen(s);
    char *p = (char *)mrb_malloc(mrb, len + 1);

    memcpy(p, s, len + 1);
    if (c->filename) {
      mrb_free(mrb, c->filename);
    }
    c->filename = p;
  }
  return c->filename;
}

MRB_API void
mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

MRB_API void
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  size_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename = mrb_sym2name_len(p->mrb, sym, NULL);
  p->lineno = (p->filename_table_length > 0)? 0 : 1;

  for (i = 0; i < p->filename_table_length; ++i) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = (int)i;
      return;
    }
  }

  p->current_filename_index = (int)p->filename_table_length++;

  new_table = (mrb_sym*)parser_palloc(p, sizeof(mrb_sym) * p->filename_table_length);
  if (p->filename_table) {
    memmove(new_table, p->filename_table, sizeof(mrb_sym) * p->current_filename_index);
  }
  p->filename_table = new_table;
  p->filename_table[p->filename_table_length - 1] = sym;
}

MRB_API char const*
mrb_parser_get_filename(struct mrb_parser_state* p, uint16_t idx) {
  if (idx >= p->filename_table_length) { return NULL; }
  else {
    return mrb_sym2name_len(p->mrb, p->filename_table[idx], NULL);
  }
}

#ifndef MRB_DISABLE_STDIO
MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = p->send = NULL;
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}
#endif

MRB_API parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = s;
  p->send = s + len;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_exec(mrb_state *mrb, struct mrb_parser_state *p, mrbc_context *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  mrb_value v;
  unsigned int keep = 0;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (c) c->parser_nerr = p->nerr;
    if (p->capture_errors) {
      char buf[256];
      int n;

      n = snprintf(buf, sizeof(buf), "line %d: %s\n",
          p->error_buffer[0].lineno, p->error_buffer[0].message);
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
    else {
      if (mrb->exc == NULL) {
        mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SYNTAX_ERROR, "syntax error"));
      }
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (proc == NULL) {
    if (mrb->exc == NULL) {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
    }
    return mrb_undef_value();
  }
  if (c) {
    if (c->dump_result) mrb_codedump_all(mrb, proc);
    if (c->no_exec) return mrb_obj_value(proc);
    if (c->target_class) {
      target = c->target_class;
    }
    if (c->keep_lv) {
      keep = c->slen + 1;
    }
    else {
      c->keep_lv = TRUE;
    }
  }
  MRB_PROC_SET_TARGET_CLASS(proc, target);
  if (mrb->c->ci) {
    mrb->c->ci->target_class = target;
  }
  v = mrb_top_run(mrb, proc, mrb_top_self(mrb), keep);
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifndef MRB_DISABLE_STDIO
MRB_API mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

MRB_API mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, size_t len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

#ifndef MRB_DISABLE_STDIO

static void
dump_prefix(node *tree, int offset)
{
  printf("%05d ", tree->lineno);
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    mrb_parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

#endif

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
#ifndef MRB_DISABLE_STDIO
  int nodetype;

  if (!tree) return;
  again:
  dump_prefix(tree, offset);
  nodetype = (int)(intptr_t)tree->car;
  tree = tree->cdr;
  switch (nodetype) {
  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(n2, offset+1);
      printf("rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(n2, offset+2);
          printf("handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("exc_var:\n");
          mrb_parser_dump(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("rescue body:\n");
          mrb_parser_dump(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("ensure:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    printf("NODE_BLOCK:\n");
    goto block;

  case NODE_BLOCK:
    block:
    printf("NODE_BLOCK:\n");
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      if (n->cdr) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n->cdr)));
      }
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("then:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (tree->car) {
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    dump_prefix(tree, offset+1);
    printf("var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(n2, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          mrb_parser_dump(mrb, n2->car, offset+3);
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("in:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("do:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
  case NODE_SCALL:
    switch (nodetype) {
    case NODE_FCALL:
      printf("NODE_FCALL:\n"); break;
    case NODE_CALL:
      printf("NODE_CALL(.):\n"); break;
    case NODE_SCALL:
      printf("NODE_SCALL(&.):\n"); break;
    default:
      break;
    }
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("method='%s' (%d)\n",
        mrb_sym2name(mrb, sym(tree->cdr->car)),
        (int)(intptr_t)tree->cdr->car);
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("::%s\n", mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3: ::%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    printf("NODE_MASGN:\n");
    dump_prefix(tree, offset+1);
    printf("mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(tree, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          if (n2->car == (node*)-1) {
            dump_prefix(n2, offset+2);
            printf("(empty)\n");
          }
          else {
            mrb_parser_dump(mrb, n2->car, offset+3);
          }
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("op='%s' (%d)\n", mrb_sym2name(mrb, sym(tree->car)), (int)(intptr_t)tree->car);
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_LVAR:
    printf("NODE_LVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    printf("NODE_MATCH:\n");
    dump_prefix(tree, offset + 1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(tree, offset + 1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", (int)(intptr_t)tree);
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%" MRB_PRId "\n", (mrb_int)(intptr_t)tree);
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    printf("NODE_INT %s base %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr->car);
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    printf("NODE_STR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DSTR:
    printf("NODE_DSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_XSTR:
    printf("NODE_XSTR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DXSTR:
    printf("NODE_DXSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    printf("NODE_DREGX\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(tree, offset);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    if (tree->cdr->cdr->cdr->car) {
      dump_prefix(tree, offset);
      printf("opt: %s\n", (char*)tree->cdr->cdr->cdr->car);
    }
    if (tree->cdr->cdr->cdr->cdr) {
      dump_prefix(tree, offset);
      printf("enc: %s\n", (char*)tree->cdr->cdr->cdr->cdr);
    }
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s (%d)\n", mrb_sym2name(mrb, sym(tree)),
           (int)(intptr_t)tree);
    break;

  case NODE_SELF:
    printf("NODE_SELF\n");
    break;

  case NODE_NIL:
    printf("NODE_NIL\n");
    break;

  case NODE_TRUE:
    printf("NODE_TRUE\n");
    break;

  case NODE_FALSE:
    printf("NODE_FALSE\n");
    break;

  case NODE_ALIAS:
    printf("NODE_ALIAS %s %s:\n",
        mrb_sym2name(mrb, sym(tree->car)),
        mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        printf(" %s", mrb_sym2name(mrb, sym(t->car)));
        t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    if (tree->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("super:\n");
      mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    printf("NODE_DEF:\n");
    dump_prefix(tree, offset+1);
    printf("%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      if (n->cdr) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n->cdr)));
      }
    }
    mrb_parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf(":%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC (<<%s):\n", ((parser_heredoc_info*)tree)->term);
    dump_recur(mrb, ((parser_heredoc_info*)tree)->doc, offset+1);
    break;

  default:
    printf("node type: %d (0x%x)\n", nodetype, (unsigned)nodetype);
    break;
  }
#endif
}
