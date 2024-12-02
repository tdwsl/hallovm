/* print out a program's args */

puts(s) {
    while(*s) sys(2, *s++);
}

main(args, argc) {
    auto i;
    if(argc) puts(args[0]);
    for(i = 1; i < argc; i++) {
        puts(", "); puts(args[i]);
    }
    puts("\n");
}
