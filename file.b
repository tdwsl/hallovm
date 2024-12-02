/* file handling functions, pretty 1:1 with syscalls */
/* note: YOU pick the file descriptor */

fopenr(fd, name) sys(4, fd, name);
fopenw(fd, name) sys(5, fd, name);
fputc(fd, b) sys(6, fd, b);
fputw(fd, w) sys(7, fd, w);
fgetc(fd) sys(8, fd);
fgetw(fd) sys(9, fd);
fclose(fd, name) sys(10, fd, name);

