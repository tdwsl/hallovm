#include <stdio.h>
#include <string.h>

unsigned short mem[65536];

void run() {
    unsigned short prev[15];
    for(int i = 0; i < 15; i++) prev[i] = mem[i]+1;

    for(;;) {
        unsigned short ins = mem[mem[15]++];
        unsigned char a = (ins>>8)&0xf, b = (ins>>4)&0xf, c = ins&0xf;
        char m = ins;

        //printf("%.4x\n", mem[15]-1);
        /*for(int i = 0; i < 15; i++) {
            if(mem[i] != prev[i]) printf("%d=%.4x ", i, mem[i]);
            prev[i] = mem[i];
        }
        printf("\n");*/

        switch(ins>>12) {
        case 0: mem[a] += m; break;
        case 1: mem[a] = mem[b] + c; break;
        case 2: mem[a] = m; break;
        case 3: mem[a] = mem[a]<<8|m&0xff; break;
        case 4: mem[a] = mem[b] + mem[c]; break;
        case 5: mem[a] = mem[b] - mem[c]; break;
        case 6: mem[a] = mem[b] & mem[c]; break;
        case 7: mem[a] = mem[b] | mem[c]; break;
        case 8: mem[a] = mem[(mem[b] + mem[c])&0xffff]; break;
        case 9: mem[(mem[b] + mem[c])&0xffff] = mem[a]; break;
        case 10: mem[a] = mem[(mem[b] + c)&0xffff]; break;
        case 11: mem[(mem[b] + c)&0xffff] = mem[a]; break;
        case 12: if(!mem[a]) mem[15] += m; break;
        case 13: if(mem[a]) mem[15] += m; break;
        case 14: mem[a] = (short)mem[b] < (short)mem[c]; break;
        default:
            switch(a) {
            case 0: mem[b] = (short)mem[b] >> (short)mem[c]; break;
            case 1: mem[b] <<= mem[c]; break;
            case 2: mem[b] ^= mem[c]; break;
            case 3: mem[b] *= mem[c]; break;
            case 4: mem[b] = (short)mem[b] / (short)mem[c]; break;
            case 5: mem[b] = (short)mem[b] % (short)mem[c]; break;
            case 6: mem[b] &= c; break;
            case 7: mem[b] |= c; break;
            case 8: mem[b] = (short)mem[b] >> c; break;
            case 9: mem[b] <<= c; break;
            case 10: mem[b] *= c; break;
            case 11: mem[b] = (short)mem[b] / c; break;
            case 12: mem[b] = -mem[c]; break;
            case 13: mem[b] = !mem[c]; break;
            case 14: mem[b] = mem[c]-1; break;
            case 15: ins = mem[15]; mem[15] = mem[14]<<8|m&0xff; mem[14] = ins; break;
            }
            break;
        }
        if(mem[16]) {
            switch(mem[16]) {
            case 1: return;
            case 2: printf("%c", mem[1]); break;
            }
            mem[16] = 0;
        }
    }
}

int main(int argc, char **args) {
    if(argc < 2) return 1;
    FILE *fp = fopen(args[1], "rb");
    if(!fp) { printf("failed to open %s\n", args[1]); return 1; }
    fread(mem+0x1000, 2, 65536-0x1000, fp);
    fclose(fp);
    int a0 = 65536-argc+2, a = a0;
    for(int i = 2; i < argc; i++) {
        mem[--a] = 0;
        char *s = args[i];
        a -= strlen(s);
        mem[a0+i-2] = a;
        for(int j = 0; s[j]; j++)
            mem[a+j] = s[j];
    }
    mem[0] = a0;
    mem[1] = argc-2;
    mem[15] = 0x1000;
    mem[13] = a-1;
    run();
    return 0;
}
