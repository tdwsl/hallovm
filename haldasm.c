#include <stdio.h>

const char *ins0[] = {
    "addi", "addir", "lit", "low", "add", "sub", "and", "or",
    "ldr", "str", "ldo", "sto", "jz", "jnz", "lt",
};

const char *ins1[] = {
    "shr", "shl", "xor", "mul", "div", "rem", "andi", "ori",
    "shri", "shli", "muli", "divi", "neg", "not", "dec", "jl",
};

void dasm(char *filename) {
    FILE *fp = fopen(filename, "rb");
    if(!fp) { printf("failed to open %s\n", filename); }
    unsigned short i, p;
    char o, a, b, c, m, s;
    unsigned short pc = 0x1000;
    while(fread(&i, 2, 1, fp)) {
        a = (i>>8)&0xf;
        b = (i>>4)&0xf;
        c = i&0xf;
        m = i;
        o = i>>12;
        printf("%.4X %.4X %c ", pc++, i, (i>32 && i<127) ? i : '.');
        if((i&0xff00) == 0x0f00) printf("jmp 0x%.4x", pc+m);
        else if(o != 0xf) {
            printf("%s ", ins0[o]);
            switch(o) {
            case 0: case 2:
                printf("r%d,%d", a, m); break;
            case 0xc: case 0xd:
                printf("r%d,0x%.4x", a, pc+m); break;
            case 3:
                printf("r%d,0x%.2x (0x%.4x)", a, m&0xff, (p<<8|m&0xff)&0xffff); break;
            case 1: case 0xa: case 0xb:
                printf("r%d,r%d,%d", a, b, c); break;
            default:
                printf("r%d,r%d,r%d", a, b, c); break;
            }
        } else {
            printf("%s ", ins1[a]);
            if(a >= 6 && a <= 0xb) printf("r%d,%d", b, c);
            else if(a == 0xf) printf("0x%.2x (0x%.4x)", m, (p<<8|m&0xff)&0xffff);
            else printf("r%d,r%d", b, c);
        }
        printf("\n");
        p = i;
    }
    fclose(fp);
}

int main(int argc, char **args) {
    for(int i = 1; i < argc; i++)
        dasm(args[i]);
    return 0;
}
