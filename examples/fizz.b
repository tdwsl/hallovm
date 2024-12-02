/* fizz buzz */

#include "put.b"

main() {
    puts("1");
    for(auto i = 2; i <= 15; i++) {
        puts(", ");
        if(i%3 == 0) puts("Fizz");
        if(i%5 == 0) puts("Buzz");
        else if(i%3 != 0) putn(i);
    }
    puts("\n");
}
