/* read a file */

#include "file.b"
#include "put.b"

main(argc, args) {
    putn(argc); puts(" args\n");
    for(auto i = 1; i < argc; i++) {
        if(!fopenr(0, args[i])) {
            puts("failed to open "); puts(args[i]); puts("\n"); return;
        }
        auto c;
        while((c = fgetc(0)) != -1) putc(c);
        fclose(0);
    }
}
