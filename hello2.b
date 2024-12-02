
putc(c) {
    sys(2, c);
}

putn(n) {
    auto a[10], p = 0;
    if(n < 0) { putc('-'); n *= -1; }
    do { a[p++] = n%10; n /= 10; } while(n);
    do putc(a[--p]+'0'); while(p);
}

main(argc, args) {
    putn(argc); putc('\n');
    putn(1017); putc('\n');
    putn(-22); putc('\n');
}
