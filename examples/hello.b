/* hello world */

puts(s) { /* see also: put.b, printf.b */
    while(*s) sys(2, *s++);
}

main() {
    puts("Hello, world!\n");
}
