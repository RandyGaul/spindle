#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE

#define CKIT_IMPLEMENTATION
#include <stddef.h>
#include <strings.h>

#ifndef _MSC_VER
#define _stricmp strcasecmp
#endif

#include "ckit.h"
#include <string.h>

typedef enum Tok
{
	TOK_EOF, TOK_IDENTIFIER, TOK_INT,

        TOK_LPAREN, TOK_RPAREN, TOK_LBRACK, TOK_RBRACK, TOK_LBRACE, TOK_RBRACE, TOK_DOT, TOK_COMMA, TOK_SEMI, TOK_QUESTION, TOK_COLON,

        TOK_IF, TOK_ELSE,

	TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH, TOK_PERCENT,
	TOK_NOT, TOK_TILDE,
	TOK_LT, TOK_LE, TOK_GT, TOK_GE, TOK_EQ, TOK_NE,
	TOK_AND_AND, TOK_OR_OR,
	TOK_ASSIGN,

	TOK_COUNT
} Tok;

const char* tok_name[TOK_COUNT] = {
	[TOK_EOF]        = "EOF",
	[TOK_IDENTIFIER] = "IDENT",
	[TOK_INT]        = "INT",

	[TOK_LPAREN]     = "(",
	[TOK_RPAREN]     = ")",
        [TOK_LBRACK]     = "[",
        [TOK_RBRACK]     = "]",
        [TOK_LBRACE]     = "{",
        [TOK_RBRACE]     = "}",
        [TOK_DOT]        = ".",
        [TOK_COMMA]      = ",",
        [TOK_SEMI]       = ";",
        [TOK_QUESTION]   = "?",
        [TOK_COLON]      = ":",

        [TOK_IF]         = "if",
        [TOK_ELSE]       = "else",

	[TOK_PLUS]       = "+",
	[TOK_MINUS]      = "-",
	[TOK_STAR]       = "*",
	[TOK_SLASH]      = "/",
	[TOK_PERCENT]    = "%",

	[TOK_NOT]        = "!",
	[TOK_TILDE]      = "~",

	[TOK_LT]         = "<",
	[TOK_LE]         = "<=",
	[TOK_GT]         = ">",
	[TOK_GE]         = ">=",
	[TOK_EQ]         = "==",
	[TOK_NE]         = "!=",

	[TOK_AND_AND]    = "&&",
	[TOK_OR_OR]      = "||",

	[TOK_ASSIGN]     = "=",
};

typedef enum Prec
{
	PREC_EXPR    = 0,
	PREC_ASSIGN  = 10,  // right-assoc
	PREC_TERNARY = 20,  // ?:
	PREC_OR_OR   = 30,
	PREC_AND_AND = 40,
	PREC_EQ      = 80,  // == !=
	PREC_REL     = 90,  // < <= > >=
	PREC_ADD     = 100, // + -
	PREC_MUL     = 110, // * / %
	PREC_POSTFIX = 120, // () [] .
	PREC_UNARY   = 130  // prefix + - ! ~
} Prec;

const char* in;
const char* at;
char ch;

struct
{
	Tok kind;
	Prec prec;
	void (*lexpr)(); // Start an expression.
	void (*rexpr)(); // Continue an expression (binary/postfix/etc).
	int int_val;
	const char* lexeme;
	int len;
} tok;

void emit_int(int v)                   { printf("EMIT push_int %d\n", v); }
void emit_ident(const char* s, int n)  { printf("EMIT push_ident \"%.*s\"\n", n, s); }
void emit_unary(Tok op)                { printf("EMIT unary %s\n", tok_name[op]); }
void emit_binary(Tok op)               { printf("EMIT binary %s\n", tok_name[op]); }
void emit_call(int argc)               { printf("EMIT call argc=%d\n", argc); }
void emit_index(void)                  { printf("EMIT index []\n"); }
void emit_member(const char* s, int n) { printf("EMIT member .%.*s\n", n, s); }
void emit_select(void)                 { printf("EMIT select ?:\n"); }
void emit_if_begin(void)               { printf("EMIT if_begin\n"); }
void emit_if_then(void)                { printf("EMIT if_then\n"); }
void emit_if_else(void)                { printf("EMIT if_else\n"); }
void emit_if_end(void)                 { printf("EMIT if_end\n"); }
void emit_block_begin(void)            { printf("EMIT block_begin\n"); }
void emit_block_end(void)              { printf("EMIT block_end\n"); }
void emit_stmt_expr(void)              { printf("EMIT stmt_expr\n"); }

void parse_error(const char* msg)
{
	fprintf(stderr, "Parse error: %s\n", msg);
	exit(1);
}

void next();
void expect(Tok k)
{
	if (tok.kind != k) parse_error("expected token");
	next();
}

void next_ch() { ch = *at ? (unsigned char)*at++ : 0; }
int is_space(int c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; }
int is_alpha(int c) { return (c == '_') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
int is_digit(int c) { return (c >= '0' && c <= '9'); }
int match_ch(int want) { if (ch == want) { next_ch(); return 1; } return 0; }

void skip_ws_comments()
{
	while (1) {
		while (is_space(ch)) next_ch();
		if (ch == '/' && at[0] == '/') {
			while (ch && ch != '\n') {
				next_ch();
			}
			continue;
		}
		if (ch == '/' && at[0] == '*') {
			next_ch();
			next_ch();
			while (ch && !(ch == '*' && at[0] == '/')) next_ch();
			if (ch == '*') {
				next_ch();
				next_ch();
			}
			continue;
		}
		break;
	}
}

void expr_binary(Prec min_prec);
void expr() { expr_binary(PREC_EXPR); }
void expr_error() { parse_error("unexpected token in expression"); }

void stmt();
void parse();

void expr_int()
{
	emit_int(tok.int_val);
	next();
}

void expr_ident()
{
	emit_ident(tok.lexeme, tok.len);
	next();
}

void expr_paren()
{
	next(); // consume '('
	expr();
	expect(TOK_RPAREN);
}

void expr_call()
{
	int argc = 0;
	if (tok.kind != TOK_RPAREN) {
		expr(); argc++;
		while (tok.kind == TOK_COMMA) { next(); expr(); argc++; }
		expect(TOK_RPAREN);
	} else {
		next(); // consume ')'
	}
	emit_call(argc);
}

void expr_index()
{
	expr(); // parse index expr
	expect(TOK_RBRACK);
	emit_index();
}

void expr_member()
{
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier after '.'");
	emit_member(tok.lexeme, tok.len);
	next();
}

void expr_ternary()
{
	// current token was '?', already consumed by Pratt loop
	expr(); // then-branch
	expect(TOK_COLON);
	expr_binary(PREC_TERNARY - 1); // else-branch
	emit_select();
}

void expr_binary_left(Tok op, Prec p)
{
	expr_binary(p);
	emit_binary(op);
}

void expr_unary_common(Tok op)
{
	next(); // consume operator
	expr_binary(PREC_UNARY - 1);
	emit_unary(op);
}

// Compact lex-parse technique learned from Per Vognsen
// https://gist.github.com/pervognsen/e61c6b91fca7275d90692831e2a55c9a
// https://gist.github.com/pervognsen/372aa279e48d58012825a66564757c40

#define EXPR_UNARY(name, token_enum) void expr_##name(void) { expr_unary_common(TOK_##token_enum); }
#define EXPR_BINARY(name, precname, token_enum) void expr_##name(void) { expr_binary_left(TOK_##token_enum, PREC_##precname); }

EXPR_UNARY(neg,  MINUS);
EXPR_UNARY(pos,  PLUS);
EXPR_UNARY(not,  NOT);
EXPR_UNARY(bnot, TILDE);

EXPR_BINARY(add,    ADD,      PLUS);
EXPR_BINARY(sub,    ADD,      MINUS);
EXPR_BINARY(mul,    MUL,      STAR);
EXPR_BINARY(div,    MUL,      SLASH);
EXPR_BINARY(mod,    MUL,      PERCENT);
EXPR_BINARY(lt,     REL,      LT);
EXPR_BINARY(le,     REL,      LE);
EXPR_BINARY(gt,     REL,      GT);
EXPR_BINARY(ge,     REL,      GE);
EXPR_BINARY(eq,     EQ,       EQ);
EXPR_BINARY(ne,     EQ,       NE);
EXPR_BINARY(land,   AND_AND,  AND_AND);
EXPR_BINARY(lor,    OR_OR,    OR_OR);
EXPR_BINARY(assign, ASSIGN,   ASSIGN);

void expr_binary(Prec min_prec)
{
        tok.lexpr(); // start: number/ident/paren/unary...

        while (tok.prec > min_prec) {
                void (*cont)() = tok.rexpr;
                next(); // consume operator -> next token begins RHS
                cont(); // parse RHS/args/member and "emit"
        }
}

void stmt_block()
{
	expect(TOK_LBRACE);
	emit_block_begin();
	while (tok.kind != TOK_RBRACE && tok.kind != TOK_EOF) {
		stmt();
	}
	expect(TOK_RBRACE);
	emit_block_end();
}

void stmt_if()
{
	expect(TOK_IF);
	emit_if_begin();
	expect(TOK_LPAREN);
	expr();
	expect(TOK_RPAREN);
	emit_if_then();
	stmt();
	if (tok.kind == TOK_ELSE) {
		next();
		emit_if_else();
		stmt();
	}
	emit_if_end();
}

void stmt_expr()
{
	expr();
	expect(TOK_SEMI);
	emit_stmt_expr();
}

void stmt()
{
	switch (tok.kind) {
	case TOK_IF:
		stmt_if();
		break;
	case TOK_LBRACE:
		stmt_block();
		break;
	case TOK_SEMI:
		next();
		break;
	case TOK_EOF:
		break;
	default:
		stmt_expr();
		break;
	}
}

void parse()
{
	while (tok.kind != TOK_EOF) {
		stmt();
	}
}



#define TOK_CHAR(ch1, tok1) \
	case ch1: next_ch(); tok.kind = TOK_##tok1; tok.prec = 0; tok.lexpr = expr_error; tok.rexpr = expr_error; break;

#define TOK_EXPR(ch1, tok1, prec1, lexpr1, rexpr1) \
	case ch1: next_ch(); tok.kind = TOK_##tok1; tok.prec = PREC_##prec1; tok.lexpr = expr_##lexpr1; tok.rexpr = expr_##rexpr1; break;

#define TOK_EXPR_EXPR(ch1, tok1, prec1, lexpr1, rexpr1, ch2, tok2, prec2, lexpr2, rexpr2) \
	case ch1: next_ch(); \
		if (match_ch(ch2)) { tok.kind = TOK_##tok2; tok.prec = PREC_##prec2; tok.lexpr = expr_##lexpr2; tok.rexpr = expr_##rexpr2; } \
		else               { tok.kind = TOK_##tok1; tok.prec = PREC_##prec1; tok.lexpr = expr_##lexpr1; tok.rexpr = expr_##rexpr1; } \
		break;

void next()
{
	tok.kind = TOK_EOF;
	tok.prec = 0;
	tok.lexpr = expr_error;
	tok.rexpr = expr_error;
	tok.lexeme = at ? at - 1 : NULL;
	tok.len = 0;

	skip_ws_comments();

        switch (ch) {
		// single-char punctuation
		TOK_CHAR(  0 , EOF)
		TOK_CHAR( ')', RPAREN)
		TOK_CHAR( ']', RBRACK)
		TOK_CHAR( '}', RBRACE)
		TOK_CHAR( ',', COMMA)
		TOK_CHAR( ';', SEMI)
		TOK_CHAR( ':', COLON)
		TOK_EXPR( '(', LPAREN,   POSTFIX, paren,  call )
		TOK_EXPR( '[', LBRACK,   POSTFIX, error,  index)
		TOK_CHAR( '{', LBRACE)
		TOK_EXPR( '.', DOT,      POSTFIX, error,  member)

		// prefix + binary-ish
		TOK_EXPR( '~', TILDE,    UNARY,   bnot,   error)
		TOK_EXPR( '+', PLUS,     ADD,     pos,    add )
		TOK_EXPR( '-', MINUS,    ADD,     neg,    sub )
		TOK_EXPR( '*', STAR,     MUL,     error,  mul )
		TOK_EXPR( '/', SLASH,    MUL,     error,  div )
		TOK_EXPR( '%', PERCENT,  MUL,     error,  mod )
		TOK_EXPR( '?', QUESTION, TERNARY, error,  ternary )

		// two-char combos
		TOK_EXPR_EXPR('<', LT,     REL,    error, lt,     '=', LE,  REL, error,   le)
		TOK_EXPR_EXPR('>', GT,     REL,    error, gt,     '=', GE,  REL, error,   ge)
		TOK_EXPR_EXPR('=', ASSIGN, ASSIGN, error, assign, '=', EQ,  EQ,  error,   eq)
		TOK_EXPR_EXPR('!', NOT,    UNARY,  not,   error,       '=', NE,  EQ,      error, ne)

		// && and ||
		TOK_EXPR_EXPR('&', NOT,    UNARY,  error, error,  '&', AND_AND,  AND_AND, error, land)
		TOK_EXPR_EXPR('|', NOT,    UNARY,  error, error,  '|', OR_OR,    OR_OR,   error, lor)

		default: break;
        }

	if (tok.kind != TOK_EOF) return;

	// identifiers
	if (is_alpha(ch)) {
		const char* s = at - 1;
		while (is_alpha(ch) || is_digit(ch)) next_ch();
		tok.len = (int)((at - 1) - s);
		tok.lexeme = s;
		tok.kind = TOK_IDENTIFIER;
		tok.prec = 0;
		tok.lexpr = expr_ident;
		tok.rexpr = expr_error;
		if (tok.len == 2 && strncmp(s, "if", 2) == 0) {
			tok.kind = TOK_IF;
			tok.lexpr = expr_error;
		}
		else if (tok.len == 4 && strncmp(s, "else", 4) == 0) {
			tok.kind = TOK_ELSE;
			tok.lexpr = expr_error;
		}
		return;
	}

	// integers (decimal)
	if (is_digit(ch)) {
		int v = (ch - '0'); next_ch();
		while (is_digit(ch)) { v = v * 10 + (ch - '0'); next_ch(); }
		tok.kind = TOK_INT;
		tok.prec = 0;
		tok.lexpr = expr_int;
		tok.rexpr = expr_error;
		tok.int_val = v;
		return;
	}

	// unknown char: consume and retry
	if (ch) {
		fprintf(stderr, "Unknown char: '%c'\n", ch);
		next_ch();
		next();
		return;
	}
}

int main(void)
{
#define STR(X) #X
        const char* input = STR(
                if (foo(1 + 2*3, a.b[4] ? y : z) && (u+v) / 2 == 7) {
                        bar();
                } else baz();
        );
        printf("Input : %s\n\n", input);

	in = input;
	at = in;
	next_ch();
	next();
        parse();

        return 0;
}
