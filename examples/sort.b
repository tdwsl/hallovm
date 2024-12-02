/* quicksort */

#include "printf.b"

putarr(a, l) {
    printf("[");
    if(l) printf("%d", a[0]);
    for(auto i = 1; i < l; i++)
        printf(", %d", a[i]);
    printf("]\n");
}

partition(a, l) {
    auto p = a[l-1];
    auto i = 0;
    for(auto j = 0; j < l-1; j++) {
        if(a[j] <= p) {
            auto t = a[i]; a[i++] = a[j]; a[j] = t;
        }
    }
    auto t = a[i]; a[i] = a[l-1]; a[l-1] = t;
    return i;
}

quicksort(a, l) {
    auto p = partition(a, l);
    if(p) {
        quicksort(a, p);
        quicksort(a+p, l-p);
    }
}

arr 10, 20, 22, 21, 13, 14, 77, 76, 9, 99, 19, 91, 1, 2, 33;
arrl = 15;

main() {
    putarr(arr, arrl);
    quicksort(arr, arrl);
    putarr(arr, arrl);
}
