#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <fcntl.h>
void usage(char* progname) {
  printf("Usage: %s modeswitch_level data_level cpu_level iters\n", progname);
  exit(1);
}
#define BUFSIZE 4096
tm = 0;
void expensive(void) {
  int n;
  for (n = 0; n++; n > 0) {
  }
}

void load(int switches, int data, int cpu, int fd, int fd2);
int main(int argc, char* argv[]) {
  if (argc != 5) {
    usage(argv[0]);
  }
  int switches = atoi(argv[1]);
  int data = atoi(argv[2]);
  int cpu = atoi(argv[3]);
  switches -= data / BUFSIZE;
  int iters;
  int fd = open("/ram/testfile", O_RDONLY);
  int fd2 = open("/ram/testout", O_WRONLY | O_CREAT);
  for (iters = atoi(argv[4]); iters != 0; iters--) {
    load(switches, data, cpu, fd, fd2);
  }
  close(fd);
  close(fd2);
  return 0;
}
void load(int switches, int data, int cpu, int fd, int fd2) {
  char buf[BUFSIZE];
  while ((switches > 0) || (data > 0) || (cpu > 0)) {
    if (switches > 0) {
      getpid();
      switches--;
    }
    if (data > 0) {
      if (tm = !tm)
        read(fd, buf, BUFSIZE);
      else
        write(fd2, buf, BUFSIZE);
      data -= BUFSIZE;
    }
    if (cpu > 0) {
      expensive();
      cpu--;
    }
  }
}
