/* kind of like leetcode #670 */

#include "printf.b"

maxSwap(n) {
    auto buf[8], l;
    auto m = n;
    for(l = 0; n; l++) {
        buf[l] = n%10; n /= 10;
    }
    for(auto i = l-1; i > 0; i--) {
        auto x = buf[i], ix;
        for(auto j = 0; j < i; j++)
            if(buf[j] > x) x = buf[ix = j];
        if(x != buf[i]) {
            buf[ix] = buf[i]; buf[i] = x;
            auto b = buf[ix];
            n = 0;
            while(l--) n = n * 10 + buf[l];
            printf("%d -> %d (swap %d and %d)\n", m, n, b, x);
            return n;
        }
    }
    printf("%d\n", m);
    return m;
}

main() {
    maxSwap(9000);
    maxSwap(9001);
    maxSwap(1002);
}

