#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* UTIL */

char *infile = 0;
int lineno;

void errh(int n) {
    if(infile) printf("%s:%d: ", infile, n);
}

void errr(unsigned short n) {
    errh(n);
}

FILE *openFile(char *filename, const char *m) {
    FILE *fp = fopen(filename, m);
    if(!fp) { errh(lineno); printf("failed to open %s\n",filename); exit(1); }
    return fp;
}

/* PARSER */

#define MAXSYMS 3000
#define SYMBUFSZ 4096
#define MAXTOKENS 3000
#define BUFSZ 500
#define MAXCONST 400
#define CONSTBUFSZ 1024

char *defsyms[] = {
    "+","-","&","|","^","*","/","%","<<",">>","<",">","=",
    "+=","-=","&=","|=","^=","*=","/=","%=","<<=",">>=",
    "!=","<=",">=","==","||","&&","?","~","!",
    "++","--","(",")","{","}","[","]",":",";", ",",
    "auto", "if","else","for","while","do","switch","case","default",
    "break","continue","return",
    "*X","&X","++X","--X","-X","+X",
    "#include",
};

enum {
    P=0,M,AND,OR,XOR,MUL,DIV,REM,SHL,SHR,LT,GT,EQ,
    PE,ME,ANDE,ORE,XORE,MULE,DIVE,REME,SHLE,SHRE,
    NOTE,LTE,GTE,EQE,OOR,AAND,TERN,INV,NOT,
    PP,MM,LP,RP,LC,RC,LB,RB,COL,SEMI,COM,
    AUTO,IF,ELSE,FOR,WHILE,DO,SWITCH,CASE,DEFAULT,
    BREAK,CONTINUE,RETURN,
    DER,REF,PPX,MMX,MX,PX,
    INCLUDE,
    NDEF,
};

short symbols[MAXSYMS]; // maybe clean up symbols at each EOF?
int nsymbols=0;

char symbuf[SYMBUFSZ];
int nsymbuf=0;

short constbuf[CONSTBUFSZ];
short consts[MAXCONST];
short constn[MAXCONST];
int nconsts = 0;
int consti = -1;
int nconsbuf = 0;

short included[50]; int nincluded = 0;

char *symbol(int s) {
    if(s == EOF) return "<EOF>";
    if((s-=NDEF) < 0) return defsyms[s+NDEF];
    if(s >= nsymbols) return "?invalid";
    return &symbuf[symbols[s]];
}

int toSymbol(char *s) {
    int i;
    for(i = 0; i < NDEF; i++) if(!strcmp(defsyms[i], s)) return i;
    for(i = 0; i < nsymbols; i++) if(!strcmp(&symbuf[symbols[i]], s)) return i+NDEF;
    strcpy(&symbuf[nsymbuf], s);
    symbols[nsymbols++] = nsymbuf;
    nsymbuf += strlen(s)+1;
    return nsymbols-1+NDEF;
}

int digit(char c) {
    if(c >= '0' && c <= '9') return (c-'0');
    if(c >= 'A' && c <= 'F') return (c-'A'+10);
    if(c >= 'a' && c <= 'f') return (c-'a'+10);
    return 0;
}

int escapeChar(char **pbuf) {
    unsigned short c, d;
    char *buf = *pbuf;
    switch(c = *(buf++)) {
    case 'n': c = 10; break;
    case 't': c = '\t'; break;
    case 'r': c = '\r'; break;
    case 'b': c = '\b'; break;
    case 'v': c = '\v'; break;
    case 'f': c = '\f'; break;
    case 'x':
        if((c = digit(*(buf++))) == -1) {
            buf--; c = 'x';
        } else if((d = digit(*(buf++))) == -1) {
            buf -= 2; c = 'x';
        } else c = c<<4|d;
        break;
    default:
        if(c >= '0' && c <= '7') {
            d = 0;
            do {
                d = d<<3 | (c-'0');
                c = *(buf++);
            } while(c >= '0' && c <= '7');
            buf--;
            c = d;
        }
        break;
    }
    *pbuf = buf;
    return c;
}

void parseString(FILE *fp, char *buf, char q) {
    char c;
    *(buf++) = q;
    for(;;) {
        c = fgetc(fp);
        if(c == q) { *buf = 0; return; }
        if(c == '\n' || c == EOF) {
            errh(lineno); printf("unterminated quote\n"); exit(1);
        }
        if(c == '\\') {
            *(buf++) = c;
            c = fgetc(fp);
            if(c == '\n' || c == EOF) {
                errh(lineno); printf("unterminated quote\n"); exit(1);
            }
        }
        *(buf++) = c;
    }
}

int extractChar(char *buf) {
    int d, c = 0;
    while(*buf) {
        if((d = *(buf++)) == '\\') d = escapeChar(&buf);
        c = c<<8|d;
    }
    return c;
}

void parseNext(FILE *fp, char *buf) {
    static char ahc = 0;
    char c;
    int i = 0;
    if(ahc) { c = ahc; ahc = 0; goto got; }
    for(;;) {
        c = fgetc(fp);
got:
        if(c == EOF) { buf[i] = 0; return; }
        if(c <= 32) {
            if(i) { ahc = c; buf[i] = 0; return; }
            if(c == 10) lineno++;
        } else if(strchr("+-&|^~!*/%<>=", c)) {
            if(i) { ahc = c; buf[i] = 0; return; }
            buf[0] = c;
            c = fgetc(fp);
            if(buf[0] == '/' && c == '*') {
                for(;;) {
                    c = fgetc(fp);
                    if(c == '*') { if((c = fgetc(fp)) == '/') break; }
                    if(c == '\n') lineno++;
                }
                continue;
            }
            if(c == '=') { buf[1] = c; buf[2] = 0; return; }
            if(buf[0] == c && strchr("<>+-&|/", c)) {
                if(c == '/') {
                    while(fgetc(fp) != '\n'); lineno++; continue;
                }
                buf[1] = c;
                if(c == '<' || c == '>') {
                    if((c = fgetc(fp)) == '=') { buf[2] = c; buf[3] = 0; }
                    else { ahc = c; buf[2] = 0; }
                } else buf[2] = 0;
                return;
            }
            if(buf[0] == '-' && c == '>') { buf[1] = '>'; buf[2] = 0; return; }
            ahc = c; buf[1] = 0; return;
        } else if(strchr("(){}[]:;,.?", c)) {
            if(i) { ahc = c; buf[i] = 0; }
            else { buf[0] = c; buf[1] = 0; }
            return;
        } else if(c == '"' || c == '\'') {
            if(i) { ahc = c; buf[i] = 0; }
            else parseString(fp, buf, c);
            return;
        } else buf[i++] = c;
    }
}

void compileFile(char *filename);

int nextSymbol(FILE *fp, char *buf) {
    if(consti != -1) {
        if(constbuf[consti] != -1)
            return constbuf[consti++];
        consti = -1;
    }
    parseNext(fp, buf);
    if(!buf[0]) return EOF;
    int sym = toSymbol(buf);
    for(int i = 0; i < nconsts; i++)
        if(consts[i] == sym) {
            consti = constn[i];
            return nextSymbol(fp, buf);
        }
    if(sym == INCLUDE) {
        int sym = nextSymbol(fp, buf);
        for(int i = 0; i < nincluded; i++)
            if(included[i] == sym) return nextSymbol(fp, buf);
        included[nincluded++] = sym;
        char *s = symbol(sym);
        if(s[0] == '"') s++;
        char *fn = infile;
        int ln = lineno;
        compileFile(s);
        lineno = ln;
        infile = fn;
        return nextSymbol(fp, buf);
    }
    return sym;
}

int parseTokens(FILE *fp, char *buf, short *tokens, unsigned short *linen) {
    static short aht = EOF;
    static int ahl;
    char f = 0;
    int d = 0;
    int ntokens = 0;
    int t;
    if(aht != EOF) { t = aht; lineno = ahl; aht = EOF; }
    else { t = nextSymbol(fp, buf); if(t == EOF) return 0; }
    goto got;
    for(;;) {
        t = nextSymbol(fp, buf);
got:
        linen[ntokens] = lineno;
        tokens[ntokens++] = t;
        if(t == RP && !(d|f)) {
            linen[ntokens] = lineno;
            tokens[ntokens++] = t = nextSymbol(fp, buf);
            if(t == LC) { f = 1; d++; continue; }
        }
        if(t == EOF) break;
        else if(t == LC) d++;
        else if(t == RC) { if(!(--d) && f) break; }
        else if(t == SEMI && !d) break;
    }
    tokens[ntokens] = EOF;
    return ntokens;
}

/* COMPILER */

#define MAXSTACK 80
#define MAXGLOBALS 1024
#define MEMSZ 32768
#define ORG 0x1000

enum {
    LIT=0x2f00,
    CALL=0x3f00,
    DIM=0x3e00,
    RET=0x0fff,
    CONT=0xfeff,
    BRK=0xfefe,
    STR=0xfefd,
};

unsigned short _mem[MEMSZ];
unsigned short *mem = _mem-ORG;
int nmem = ORG;
unsigned short data = MEMSZ;
unsigned short start;
struct global { short sym; short a; } globals[MAXGLOBALS];
int nglobals = 0;
short locals[50]; int nlocals, maxlocals, maxdim, dim;
int loffs = 0;
short strings[80]; int nstrings;

typedef struct tocon {
    short tokens[MAXTOKENS]; unsigned short linen[MAXTOKENS];
    int i;
    short stack[MAXSTACK]; int sp;
} tocon;

void insertSym(tocon *con, int i0, short sym) {
    for(int i = con->sp++; i > i0; i--) con->stack[i] = con->stack[i-1];
    con->stack[i0] = sym;
}

int precedence(short op) {
    if(op == DIV || op == MUL || op == REM) return 1;
    if(op == P || op == M) return 2;
    if(op == SHL || op == SHR) return 3;
    if(op == GT || op == LT || op == GTE || op == LTE) return 4;
    if(op == EQE || op == NOTE) return 5;
    if(op == AND) return 6;
    if(op == XOR) return 7;
    if(op == OR) return 8;
    if(op == AAND) return 9;
    if(op == OOR) return 10;
    if(op == TERN) return 11;
    if(op >= EQ && op <= SHRE) return 12;
    if(op == COM) return 13;
    return 0;
}

void parseComma(tocon *con);

void parsePost(tocon *con) {
    if(con->tokens[con->i] == PP) {
        con->i++; parsePost(con); con->stack[con->sp++] = PP;
    } else if(con->tokens[con->i] == MM) {
        con->i++; parsePost(con); con->stack[con->sp++] = MM;
    } else if(con->tokens[con->i] == LP) {
        con->i++;
        if(con->tokens[con->i] == RP) con->stack[con->sp++] = RP;
        else {
            parseComma(con); con->stack[con->sp++] = LP;
            if(con->tokens[con->i] != RP) {
                errr(con->linen[con->i]); printf("expected closing )\n"); exit(1);
            }
        }
        con->i++; parsePost(con);
    } else if(con->tokens[con->i] == LB) {
        con->i++; parseComma(con); con->stack[con->sp++] = LB;
        if(con->tokens[con->i] != RB) {
            errr(con->linen[con->i]); printf("expected closing ]\n"); exit(1);
        }
        con->i++; parsePost(con);
    }
}

void parsePre(tocon *con) {
    if(con->tokens[con->i] == INV) {
        con->i++; parsePre(con); con->stack[con->sp++] = INV;
    } else if(con->tokens[con->i] == NOT) {
        con->i++; parsePre(con); con->stack[con->sp++] = NOT;
    } else if(con->tokens[con->i] == PP) {
        con->i++; parsePre(con); con->stack[con->sp++] = PPX;
    } else if(con->tokens[con->i] == MM) {
        con->i++; parsePre(con); con->stack[con->sp++] = MMX;
    } else if(con->tokens[con->i] == AND) {
        con->i++; parsePre(con); con->stack[con->sp++] = REF;
    } else if(con->tokens[con->i] == MUL) {
        con->i++; parsePre(con); con->stack[con->sp++] = DER;
    } else if(con->tokens[con->i] == P) {
        con->i++; parsePre(con); con->stack[con->sp++] = PX;
    } else if(con->tokens[con->i] == M) {
        con->i++; parsePre(con); con->stack[con->sp++] = MX;
    } else {
        if(con->tokens[con->i] == LP) {
            con->i++; parseComma(con);
            if(con->tokens[con->i] != RP) {
                errr(con->linen[con->i]); printf("expected closing )\n"); exit(1);
            }
            con->i++;
        } else con->stack[con->sp++] = con->tokens[con->i++];
        parsePost(con);
    }
}

void parseRest(tocon *con, int p) {
    short t;
    if(!p) parsePre(con);
    else {
        parseRest(con, p-1);
        while(precedence(t = con->tokens[con->i]) == p) {
            con->i++;
            parseRest(con, p-1);
            con->stack[con->sp++] = t;
        }
    }
}

void parseTern(tocon *con) {
    parseRest(con, 10);
    if(con->tokens[con->i] == TERN) {
        con->i++; parseTern(con);
        if(con->tokens[con->i] != COL) {
            errr(con->linen[con->i]); printf("expected : after ?\n"); exit(1);
        }
        con->i++; parseTern(con); con->stack[con->sp++] = TERN;
    }
}

void parseAss(tocon *con) {
    int t;
    parseTern(con);
    if((t = con->tokens[con->i]) >= PE && t <= SHRE || t == EQ) {
        con->i++; parseAss(con); con->stack[con->sp++] = t;
    }
}

void parseComma(tocon *con) {
    parseAss(con);
    if(con->tokens[con->i] == COM) {
        con->i++; parseComma(con);
        con->stack[con->sp++] = COM;
    }
}

void parseExpr(tocon *con) {
    con->sp = 0;
    parseComma(con);
}

int hex(char *s, int *n) {
    do {
        *n <<= 4;
        if(*s >= '0' && *s <= '9') *n |= *s - '0';
        else if(*s >= 'a' && *s <= 'f') *n |= *s - 'a' + 10;
        else if(*s >= 'A' && *s <= 'F') *n |= *s - 'A' + 10;
        else return 0;
    } while(*(++s));
    return 1;
}

int oct(char *s, int *n) {
    do {
        if(*s >= '0' && *s <= '7') *n = *n << 3 | (*s - '0');
        else return 0;
    } while(*(++s));
    return 1;
}

int number(char *s, int *n) {
    *n = 0;
    if(*s == '0') {
        if(s[1] == 'x') return hex(s+2, n);
        return oct(s, n);
    }
    do {
        if(*s >= '0' && *s <= '9') *n = *n * 10 + *s - '0';
        else return 0;
    } while(*(++s));
    return 1;
}

int reduceExp(short *ex, int i, int *n) {
    if(!i) return i;
    if(number(symbol(ex[i-1]), n)) return i-1;
    if(symbol(ex[i-1])[0] == '\'') {
        *n = extractChar(symbol(ex[i-1])+1); return i-1;
    }
    int a, b, i0 = i, i1 = i-1;
    short op = ex[i1];
    if((i = reduceExp(ex, i1, n)) == i1) return i0;
    switch(op) {
    case MX: *n = -*n; return i;
    case NOT: *n = !*n; return i;
    case INV: *n = ~*n; return i;
    }
    i1 = i;
    if((i = reduceExp(ex, i, &b)) == i1) return i0;
    switch(op) {
    case P: *n += b; return i;
    case M: *n -= b; return i;
    case OOR: *n = *n || b; return i;
    case AAND: *n = *n && b; return i;
    case OR: *n |= b; return i;
    case AND: *n &= b; return i;
    case XOR: *n ^= b; return i;
    case DIV: *n = b / *n; return i;
    case REM: *n = b % *n; return i;
    case MUL: *n *= b; return i;
    case SHR: *n = b >> *n; return i;
    case SHL: *n = b << *n; return i;
    case EQE: *n = *n == b; return i;
    case NOTE: *n = *n != b; return i;
    case GT: *n = b > *n; return i;
    case LT: *n = b < *n; return i;
    case GTE: *n = b >= *n; return i;
    case LTE: *n = b <= *n; return i;
    case TERN:
        i1 = i;
        if((i = reduceExp(ex, i, &a)) == i1) return i0;
        *n = a ? b : *n;
        return i;
    }
    return i0;
}

void checkLval() {
    if((mem[nmem-1]&0xf000) != 0x8000 && (mem[nmem-1]&0xf000) != 0xa000) {
        errr(start); printf("expected lvalue\n"); exit(1);
    }
}

int findGlobal(short sym) {
    for(int i = 0; i < nglobals; i++)
        if(globals[i].sym == sym) return i;
    return -1;
}

int findLocal(short sym) {
    for(int i = nlocals-1; i >= 0; i--)
        if(locals[i] == sym) return i+loffs;
    return -1;
}

void pushRegs(int rn) {
    if(!rn) return;
    mem[nmem++] = 0x0d00|-rn&0xff;
    for(int i = 0; i < rn; i++)
        mem[nmem++] = 0xb0d0|rn<<8;
    loffs += rn;
}

void popRegs(int rn) {
    if(!rn) return;
    mem[nmem++] = rn<<8;
    for(int i = 0; i < rn; i++)
        mem[nmem++] = 0xa0d0|rn<<8;
    mem[nmem++] = 0x0d00|rn;
    loffs -= rn;
}

int compileExpr0(short *ex, int i, int rn);

int compileCall(short *ex, int i) {
    i = compileExpr0(ex, i, 0);
    if((mem[nmem-2]&0xff00) == LIT) mem[nmem-2] = CALL|mem[nmem-2]&0xff;
    else if((mem[nmem-2]&0xff00) == 0x2000
            && (mem[nmem-1]&0xff00) == 0x3000) {
        mem[nmem-2] = mem[nmem-2]|0x0e00;
        mem[nmem-1] = mem[nmem-1]&0x00ff|0xff00;
    } else if((mem[nmem-1]&0xf000) == 0x8000 || (mem[nmem-1]&0xf000) == 0xa000) {
        mem[nmem] = mem[nmem-1]|0x0e00;
        mem[nmem++-1] = 0x1ef1;
    } else {
        mem[nmem++] = 0x1ef1;
        mem[nmem++] = 0x1f00;
    }
    return i;
}

void compileLocal(int n, int rn) {
    if(n <= 15) mem[nmem++] = 0xa0d0|rn<<8|n;
    else {
        mem[nmem++] = 0x2000|rn<<8|n;
        mem[nmem++] = 0x80d0|rn<<8|rn;
    }
}

int compileExpr0(short *ex, int i, int rn) {
    int n, o;
    char rb;
    short l = 0;
    short cbuf[20];
    if(!i) return 0;
    if((o = reduceExp(ex, i, &n)) != i) {
        printf("rn:%d\n", rn);
        if(n >= -128 && n <= 127)
            mem[nmem++] = 0x2000|rn<<8|n&0xff;
        else {
            mem[nmem++] = 0x2000|rn<<8|(n>>8)&0xff;
            mem[nmem++] = 0x3000|rn<<8|n&0xff;
        }
        return o;
    }
    o = ex[i-1];
    if(o >= NDEF) {
        if(symbol(o)[0] == '"') {
            mem[nmem++] = STR;
            mem[nmem++] = rn<<8|nstrings;
            strings[nstrings++] = o;
        } else if((n = findLocal(o)) != -1) {
            compileLocal(n, rn);
        } else if((n = findGlobal(o)) != -1) {
            struct global *g = &globals[n];
            if(g->a < -1) {
                mem[nmem++] = 0x2000|rn<<8|(-g->a>>8)&0xff;
                mem[nmem++] = 0x3000|rn<<8|-g->a&0xff;
            } else if(g->a > 0) {
                mem[nmem++] = 0x2000|rn<<8|(-g->a>>8)&0xff;
                mem[nmem++] = 0x3000|rn<<8|-g->a&0xff;
                mem[nmem++] = 0xa000|rn<<8|rn<<4;
            } else if(g->a) {
                mem[nmem++] = LIT|(n>>8)&0xff;
                mem[nmem++] = rn<<8|n&0xff;
            } else {
                mem[nmem++] = LIT|(n>>8)&0xff;
                mem[nmem++] = rn<<8|n&0xff;
                mem[nmem++] = 0xa000|rn<<8|rn<<4;
            }
        } else {
            struct global *g = &globals[nglobals++];
            g->sym = o;
            g->a = 0;
            mem[nmem++] = LIT|(n>>8)&0xff;
            mem[nmem++] = rn<<8|n&0xff;
            mem[nmem++] = 0xa000|rn<<8|rn<<4;
        }
        return i-1;
    }
    switch(o) {
    case MX:
        i = compileExpr0(ex, i-1, rn);
        mem[nmem++] = 0xfc00|rn<<4|rn;
        return i;
    case PX:
        return i;
    case NOT:
        i = compileExpr0(ex, i-1, rn);
        mem[nmem++] = 0xfd00|rn<<4|rn;
        return i;
    case DER:
        i = compileExpr0(ex, i-1, rn);
        if((mem[nmem-1]&0xf000) == 0x1000) mem[nmem-1] += 0x4000;
        else if((mem[nmem-1]&0xf000) == 0x4000) mem[nmem-1] += 0xa000;
        else if(!(mem[nmem-1]&0xf000) && (o = (char)mem[nmem-1]) >= -8 && o <= 7)
            mem[nmem-1] = 0xa000|rn<<8|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0xa000|rn<<8|rn<<4;
        return i;
    case REF:
        i = compileExpr0(ex, i-1, rn);
        if((o = mem[nmem-1]>>12) == 0x8)
            mem[nmem-1] = mem[nmem-1]&0x0fff|0x4000;
        else if(o == 0xa)
            mem[nmem-1] = mem[nmem-1]&0x0fff|0x1000;
        else { errr(start); printf("expected lvalue\n"); exit(1); }
        return i;
    case PPX:
        i = compileExpr0(ex, i-1, rn+1);
        checkLval();
        mem[nmem-1] = mem[nmem-1]&0xf0ff|rn<<8;
        mem[nmem++] = rn<<8|1;
        mem[nmem] = mem[nmem-2]+0x1000; nmem++;
        return i;
    case MMX:
        i = compileExpr0(ex, i-1, rn+1);
        checkLval();
        mem[nmem-1] = mem[nmem-1]&0xf0ff|rn<<8;
        mem[nmem++] = rn<<8|0xff;
        mem[nmem] = mem[nmem-2]+0x1000; nmem++;
        return i;
    case PP:
        i = compileExpr0(ex, i-1, rn+1);
        checkLval();
        mem[nmem-1] = mem[nmem-1]&0xf0ff|rn<<8;
        mem[nmem++] = 0x1000|14<<8|rn<<4|1;
        mem[nmem] = mem[nmem-2]+0x1000&0xf0ff|14<<8; nmem++;
        return i;
    case MM:
        i = compileExpr0(ex, i-1, rn+1);
        checkLval();
        mem[nmem-1] = mem[nmem-1]&0xf0ff|rn<<8;
        mem[nmem++] = 0xfe00|14<<4|rn;
        mem[nmem] = mem[nmem-2]+0x1000&0xf0ff|14<<8; nmem++;
        return i;
    case EQ:
        i = compileExpr0(ex, i-1, rn);
        i = compileExpr0(ex, i, rn+1);
        checkLval();
        mem[nmem-1] = (mem[nmem-1]&0xf0ff)+0x1000|rn<<8;
        return i;
    case AAND:
        i = compileExpr0(ex, i-1, rn);
        n = nmem++;
        i = compileExpr0(ex, i, rn);
        mem[n] = 0xc000|nmem-n-1;
        return i;
    case OOR:
        i = compileExpr0(ex, i-1, rn);
        n = nmem++;
        i = compileExpr0(ex, i, rn);
        mem[n] = 0xd000|nmem-n-1;
        return i;
    case RP:
        pushRegs(rn);
        i = compileCall(ex, i-1);
        popRegs(rn);
        return i;
    case LP:
        pushRegs(rn);
        n = 1;
        while(ex[--i-1] == COM) n++;
        while(n--) i = compileExpr0(ex, i, n);
        i = compileCall(ex, i);
        popRegs(rn);
        return i;
    }
    int bi;
    n = nmem;
    if(o == TERN) {
        nmem++;
        i = compileExpr0(ex, i-1, rn);
        mem[n] = 0xd000|nmem-n;
        o = nmem++;
        i = compileExpr0(ex, i, rn);
        mem[o] = 0x0f00|nmem-o-1;
        o = TERN;
    } else i = compileExpr0(ex, bi=i-1, rn+(o!=COM));
    for(int j = n; j < nmem; j++) cbuf[j-n] = mem[j];
    n = nmem - n; nmem -= n;
    rb = rn + 1;
    if(o >= PE && o <= SHRE) {
        o = o - PE + P;
        i = compileExpr0(ex, i, rn+1);
        checkLval();
        mem[nmem-1] = mem[nmem-1]&0xf0ff|rn<<8;
        l = mem[nmem-1];
        if((l&0x00f0) != 0x00d0) rb++;
        if((l&0xf000) == 0x8000) rb++;
        compileExpr0(ex, bi, rb);
    } else {
        i = compileExpr0(ex, i, rn);
        for(int j = 0; j < n; j++) mem[nmem++] = cbuf[j];
    }
    switch(o) {
    case P:
        if((mem[nmem-1]&0xf000) == 0x2000)
            mem[nmem-1] = rn<<8|mem[nmem-1]&0xff;
        else mem[nmem++] = 0x4000|rn<<8|rn<<4|rb;
        break;
    case M:
        if((mem[nmem-1]&0xf000) == 0x2000)
            mem[nmem-1] = rn<<8|(-mem[nmem-1])&0xff;
        else mem[nmem++] = 0x5000|rn<<8|rn<<4|rb;
        break;
    case MUL:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0xf0))
            mem[nmem-1] = 0xfa00|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0xf300|rn<<4|rb;
        break;
    case DIV:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0xf0))
            mem[nmem-1] = 0xfb00|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0xf400|rn<<4|rb;
        break;
    case REM:
        mem[nmem++] = 0xf500|rn<<4|rb;
        break;
    case SHR:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0xf0))
            mem[nmem-1] = 0xf800|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0xf000|rn<<4|rb;
        break;
    case SHL:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0xf0))
            mem[nmem-1] = 0xf900|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0xf100|rn<<4|rb;
        break;
    case AND:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0xf0))
            mem[nmem-1] = 0xf600|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0x6000|rn<<8|rn<<4|rb;
        break;
    case OR:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0xf0))
            mem[nmem-1] = 0xf700|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0x7000|rn<<8|rn<<4|rb;
        break;
    case XOR:
        mem[nmem++] = 0xf200|rn<<4|rb;
        break;
    case EQE:
        if((mem[nmem-1]&0xf000) == 0x2000)
            mem[nmem-1] = (-mem[nmem-1])&0xff|rn<<8;
        else mem[nmem++] = 0x4000|rn<<8|rn<<4|rb;
        mem[nmem] = mem[nmem+1] = 0xd000|rn<<8|rn<<4|rb;
        nmem += 2;
        break;
    case NOTE:
        if((mem[nmem-1]&0xf000) == 0x2000)
            mem[nmem-1] = (-mem[nmem-1])&0xff|rn<<8;
        else mem[nmem++] = 0x4000|rn<<8|rn<<4|rb;
        mem[nmem++] = 0xfd00|rn<<4|rb;
        break;
    case LT:
        mem[nmem++] = 0xe000|rn<<8|rn<<4|rb;
        break;
    case GT:
        mem[nmem++] = 0xe000|rn<<8|rb<<4|rn;
        break;
    case LTE:
        mem[nmem++] = 0x00ff|rn<<8;
        mem[nmem++] = 0xe000|rn<<8|rn<<4|rb;
        break;
    case GTE:
        mem[nmem++] = 0x00ff|rn<<8;
        mem[nmem++] = 0xe000|rn<<8|rb<<4|rn;
        break;
    case LB:
        if((mem[nmem-1]&0xf000) == 0x2000 && !(mem[nmem-1]&0x00f0))
            mem[nmem-1] = 0xa000|rn<<8|rn<<4|mem[nmem-1]&0xf;
        else mem[nmem++] = 0x8000|rn<<8|rn<<4|rb;
        break;
    }
    if(l) mem[nmem++] = (l&0xf0ff)+0x1000|rn<<8;
    return i;
}

void printStack(tocon *con) {
    printf("ex:");
    for(int i = 0; i < con->sp; i++) printf(" %s", symbol(con->stack[i]));
    printf("\n");
    int n;
    printf("reduce: ");
    if(reduceExp(con->stack, con->sp, &n) != con->sp) printf("%d", n);
    else printf("no");
    printf("\n\n");
}

void compileExpr(tocon *con) {
    start = con->linen[con->i];
    parseExpr(con);
    printStack(con);
    compileExpr0(con->stack, con->sp, 0);
}

void compileString(char *s) {
    for(;;) {
        if(*s == '\\') { s++; mem[nmem++] = escapeChar(&s); }
        else { mem[nmem++] = *s; if(!*s++) break; }
    }
}

void id(unsigned short line, short sym) {
    int n;
    if(sym < NDEF || number(symbol(sym), &n)) {
        errr(line); printf("invalid identifier %s\n", symbol(sym)); exit(1);
    }
}

void expect(tocon *con, short t) {
    if(con->tokens[con->i] != t) {
        errr(con->linen[con->i]); printf("expected %s\n", symbol(t)); exit(1);
    }
    con->i++;
}

void resolveBreaks(int i, int cad) {
    for(; i < nmem; i++) {
        if(mem[i] == BRK) mem[i] = 0x0f00|(nmem-i-1)&0xff;
        else if(mem[i] == CONT) mem[i] = 0x0f00|(cad-i-1)&0xff;
    }
}

void compileStatement(tocon *con) {
    switch(con->tokens[con->i++]) {
    case LC:
        {
        int n = nlocals;
        int d = dim;
        for(;;) {
            if(con->tokens[con->i] == RC) break;
            compileStatement(con);
        }
        if(dim > maxdim) maxdim = dim;
        dim = d;
        if(nlocals > maxlocals) maxlocals = nlocals;
        nlocals = n;
        con->i++;
        return;
        }
    case RETURN:
        compileExpr(con);
        mem[nmem++] = RET;
        break;
    case AUTO:
        for(;;) {
            id(con->linen[con->i], locals[nlocals++] = con->tokens[con->i]);
            if(con->tokens[con->i+1] == EQ) {
                start = con->linen[con->i]; con->sp = 0;
                parseAss(con);
                compileExpr0(con->stack, con->sp, 0);
            } else if(con->tokens[con->i+1] == LB) {
                start = con->linen[con->i]; con->sp = 0;
                con->i++; parsePost(con);
                int n;
                if(con->sp != 2 || !number(symbol(con->stack[0]), &n)) {
                    errr(con->linen[con->i]);
                    printf("expected array dimension\n"); exit(1);
                }
                mem[nmem++] = DIM|(dim>>8)&0xff;
                mem[nmem++] = 0x3000|dim&0xff;
                mem[nmem++] = 0x400d;
                compileLocal(nlocals-1, 0);
                mem[nmem-1] += 0x1000;
                dim += n;
            } else con->i++;
            if(con->tokens[con->i] != COM) break;
            con->i++;
        }
        break;
    case IF:
        {
        expect(con, LP);
        compileExpr(con);
        expect(con, RP);
        int a = nmem++;
        compileStatement(con);
        if(con->tokens[con->i] == ELSE) {
            con->i++;
            int b = nmem++;
            compileExpr(con);
            mem[b] = 0x0f00|nmem-b;
        }
        mem[a] = 0xc000|nmem-a-1;
        return;
        }
    case WHILE:
        {
        int a0 = nmem;
        expect(con, LP);
        compileExpr(con);
        expect(con, RP);
        int a1 = nmem++;
        compileStatement(con);
        mem[nmem] = 0x0f00|(a0-nmem-1)&0xff; nmem++;
        mem[a1] = 0xc000|nmem-a1-1;
        resolveBreaks(a1+1, a0);
        return;
        }
    case DO:
        {
        int a0 = nmem;
        compileStatement(con);
        expect(con, WHILE);
        expect(con, LP);
        compileExpr(con);
        expect(con, RP);
        mem[nmem] = 0xd000|(a0-nmem-1)&0xff; nmem++;
        break;
        }
    case BREAK:
        mem[nmem++] = BRK;
        break;
    case CONTINUE:
        mem[nmem++] = CONT;
        break;
    case FOR:
        {
        expect(con, LP);
        compileExpr(con);
        expect(con, SEMI);
        int a0 = nmem;
        compileExpr(con);
        int a1 = nmem++;
        expect(con, SEMI);
        int i0 = con->i;
        parseExpr(con);
        expect(con, RP);
        compileStatement(con);
        int a2 = nmem;
        int i1 = con->i;
        con->i = i0;
        compileExpr(con);
        con->i = i1;
        mem[nmem] = 0x0f00|(a0-nmem-1)&0xff; nmem++;
        mem[a1] = 0xc000|nmem-a1-1;
        resolveBreaks(a1+1, a0);
        return;
        }
    default:
        con->i--;
        compileExpr(con);
        break;
    }
    if(con->tokens[con->i++] != SEMI) {
        errr(con->linen[con->i-1]); printf("expected ; at end of statement\n"); exit(1);
    }
}

void scooch(int a0, int sz) {
    for(int i = (nmem += sz)-1; i >= a0+sz; i--)
        mem[i] = mem[i-sz];
}

void compileFunction(tocon *con, struct global *g) {
    nmem--;
    g->a = -g->a;
    locals[0] = RETURN;
    nlocals = 1; con->i++;
    if(con->tokens[con->i] != RP)
        for(;;) {
            id(con->linen[con->i], locals[nlocals++] = con->tokens[con->i]);
            if(con->tokens[++con->i] == RP) break;
            expect(con, COM);
        }
    con->i++;
    maxlocals = nlocals;
    dim = maxdim = 0;
    nstrings = 0;
    printf("locals are:");
    for(int i = 0; i < nlocals; i++) printf(" %s", symbol(locals[i]));
    printf("\n");
    int a0 = nmem;
    mem[nmem++] = 0;
    mem[nmem++] = 0xbed0;
    for(int i = 1; i < nlocals; i++)
        mem[nmem++] = 0xb0d0|(i-1)<<8|i;
    compileStatement(con);
    if(mem[nmem-1] == RET) nmem--;
    for(int i = -g->a; i < nmem; i++) {
        if(mem[i] == RET) mem[i] = 0x0f00|nmem-i-1;
        else if((mem[i]&0xff00) == DIM) {
            int d = ((mem[i]&0xff)<<8|mem[i+1]&0xff)+maxlocals;
            mem[i] = 0x2000|d>>8;
            mem[i+1] = 0x3000|d&0xff;
        }
    }
    mem[nmem++] = 0xaed0;
    maxlocals += maxdim;
    //if(maxlocals) {
        mem[a0] = 0x0d00|-maxlocals&0xff;
        mem[nmem++] = 0x0d00|maxlocals;
    //}
    mem[nmem++] = 0x1fe0;
    for(int i = -g->a; i < nmem; i++)
        if(mem[i] == STR) {
            mem[i] = 0x2000|mem[i+1]&0x0f00|nmem>>8; i++;
            int s = strings[mem[i]&0xff];
            mem[i] = 0x3000|mem[i]&0x0f00|nmem&0xff;
            compileString(symbol(s)+1);
        }
}

void compileFile(char *filename);

void compileGlobal(tocon *con) {
    struct global *g;
    int n, i, j;
    id(con->linen[0], con->tokens[0]);
    if(con->tokens[1] == EQ) {
        consts[nconsts] = con->tokens[0];
        constn[nconsts++] = nconsbuf;
        for(int i = 2; con->tokens[i] != SEMI; i++)
            constbuf[nconsbuf++] = con->tokens[i];
        constbuf[nconsbuf++] = -1;
        return;
    }
    if((i = findGlobal(con->tokens[0])) != -1) {
        g = &globals[i];
        if(g->a == -1) {
            if(con->tokens[1] != LP) {
                errr(con->linen[1]);
                printf("%s expected to be function\n", symbol(g->sym)); exit(1);
            }
        } else if(g->a) {
            errr(con->linen[con->i-1]); printf("%s already defined\n", symbol(g->sym));
            exit(1);
        }
        g->a = nmem++;
    } else {
        g = &globals[nglobals++];
        g->sym = con->tokens[0];
    }
    con->i++;
    g->a = nmem++;
    if(con->tokens[1] == SEMI) return;
    if(con->tokens[1] == LP) compileFunction(con, g);
    else if(con->tokens[1] == LB) {
        con->i++; parseExpr(con);
        if(con->tokens[1] != RB) {
            errr(con->linen[con->i]); printf("expected closing ]\n"); exit(1);
        }
        if(con->tokens[con->i+1] != SEMI) {
            errr(con->linen[con->i+1]); printf("expected ;\n"); exit(1);
        }
        if(reduceExp(con->stack, con->sp, &n) == con->sp) {
            errr(con->linen[con->i]);
            printf("array dimension must be a constant\n"); exit(1);
        }
        mem[g->a] = data; data += n;
    } else {
        parseExpr(con);
        printf("%s\n", symbol(con->stack[con->sp-1]));
        if(con->stack[con->sp-1] == COM) {
            mem[g->a] = nmem++;
            while(con->stack[--con->sp] == COM) nmem++;
            i = ++con->sp;
            printStack(con);
            j = nmem-1;
            do {
                if(symbol(con->stack[i-1])[0] == '"') {
                    mem[j] = nmem;
                    compileString(symbol(con->stack[--i])+1);
                } else {
                    int i0 = i;
                    if((i = reduceExp(con->stack, i, &n)) == i0) {
                        errr(con->linen[con->i-1]);
                        printf("expected constant\n"); exit(1);
                    }
                    mem[j] = n;
                }
                j--;
            } while(i);
        } else {
            if(con->sp == 1 && symbol(con->stack[0])[0] == '"') {
                mem[g->a] = nmem;
                compileString(symbol(con->stack[0])+1);
            } else {
                i = con->sp;
                if(reduceExp(con->stack, con->sp, &n) == con->sp) {
                    errr(con->linen[con->i-1]); printf("expected constant\n");
                    exit(1);
                }
                mem[g->a] = n;
            }
        }
    }
}

void compileFile(char *filename) {
    FILE *fp = openFile(filename, "r");
    short c;
    int ntokens;
    char buf[BUFSZ];
    struct tocon con;
    lineno = 1; infile = filename;
    for(;;) {
        ntokens = parseTokens(fp, buf, con.tokens, con.linen);
        if(!ntokens) break;
        printf("%d %s", con.linen[0], symbol(con.tokens[0]));
        for(int i = 1; i < ntokens; i++) {
            if(con.linen[i] != con.linen[i-1]) printf("\n%d", con.linen[i]);
            printf(" %s", symbol(con.tokens[i]));
        }
        printf("\n\n");
        con.i = 0;
        compileGlobal(&con);
    }
    fclose(fp);
}

void saveFile(char *filename) {
    for(int i = 0; i < nglobals; i++) {
        struct global *g = &globals[i];
        if(g->a == 0 || g->a == -1) {
            printf("error: unresolved reference to %s\n", symbol(g->sym)); exit(1);
        } else if(g->a < 0) {
            for(int j = -g->a; mem[j] != 0x1fe0; j++)
                if((mem[j]&0xff00) == LIT) {
                    g = &globals[(mem[j]&0xff)<<8|mem[j+1]&0xff];
                    int n = g->a; if(n < 0) n = -n;
                    mem[j] = 0x2000|mem[j+1]&0x0f00|(n>>8)&0xff;
                    j++; mem[j] = 0x3000|mem[j]&0x0f00|n&0xff;
                } else if((mem[j]&0xff00) == CALL) {
                    g = &globals[(mem[j]&0xff)<<8|mem[j+1]&0xff];
                    mem[j] = mem[j]&0x00ff|0x2e00;
                    j++; mem[j] = 0xff00|mem[j]&0xff;
                    mem[j] = 0x2e00|(-g->a>>8)&0xff;
                    mem[++j] = 0xff00|-g->a&0xff;
                }
        }
    }
    int a = findGlobal(toSymbol("main"));
    if(a == -1) { printf("error: main not found\n"); exit(1); }
    a = globals[a].a;
    if(a > 0) { printf("error: main is not a function\n"); exit(1); }
    a = -a;
    _mem[0] = 0x2e00|a>>8&0xff;
    _mem[1] = 0xff00|a&0xff;
    FILE *fp = openFile(filename, "wb");
    fwrite(_mem, 2, nmem-ORG, fp);
    fclose(fp);
}

/* MAIN */

char *outfile = "a.out";

void option(char *s) {
    switch(*(s++)) {
    case 'o': outfile = s; break;
    default: printf("unknown option '%c'\n", *(s-1)); break;
    }
}

int main(int argc, char **args) {
    int i, j;
    for(i = 1; i < argc; i++)
        if(*args[i] == '-') { option(args[i]+1); argc--;
            for(j = i--; j < argc; j++) args[j] = args[j+1];
        }
    if(argc == 1) return 1;
    nmem = ORG+6;
    _mem[2] = 0x2001;
    _mem[3] = 0x2c10;
    _mem[4] = 0xb0c0;
    _mem[5] = 0x1fe0;
    globals[nglobals].sym = toSymbol("sys");
    globals[nglobals++].a = -ORG-3;
    for(i = 1; i < argc; i++) compileFile(args[i]);
    saveFile(outfile);
    return 0;
}

