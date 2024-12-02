/* pretty basic printf, supports strings, chars, integers */

pputn(putc, n) {
    auto a[10], p = 0;
    if(n < 0) { putc('-'); n *= -1; }
    do { a[p++] = n%10; n /= 10; } while(n);
    do putc(a[--p]+'0'); while(p);
}

pputs(putc, s) {
    while(*s) putc(*s++);
}

sprintf_buf;
sprintf_putc(c) {
    *(sprintf_buf++) = c;
}

pprintf(putc, s, nx) {
    while(*s) {
        auto c = *(s++);
        if(c == '%') {
            c = *(s++);
            if(c == 'd') pputn(putc, *(nx++));
            else if(c == 'c') putc(*(nx++));
            else if(c == 's') pputs(putc, *(nx++));
            else putc(c);
        } else putc(c);
    }
}

printf_putc(c) {
    sys(2, c);
}

printf(s, a,b,c,d,e,f,g,h,i,j,k,l) {
    pprintf(printf_putc, s, &a);
}

sprintf(buf, s, a,b,c,d,e,f,g,h,i,j,k,l) {
    sprintf_buf = buf;
    pprintf(sprintf_putc, s, &a);
    *sprintf_buf = 0;
}

