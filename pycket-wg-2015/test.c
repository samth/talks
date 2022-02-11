#include <time.h>
#include <stdio.h>
#include <stdlib.h>


int main() {

  clock_t begin, end;
  double time_spent;
  
  double* array = malloc(10000000*sizeof(double));
  double* array2 = malloc(10000000*sizeof(double));

  int N = 10000000;
  int i;
  srand((unsigned int)15);
  
  double a = 5.0;
  for (i=0;i<N;i++) {
    array[i] = ((double)rand()/(double)(RAND_MAX));
    array2[i] = ((double)rand()/(double)(RAND_MAX));
  }

  begin = clock();

  double acc = 0.0;
  for (i=0;i<N;i++) {
    acc += array[i] * array2[i];
  }

  end = clock();
  time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

  printf("time spent: %f %f\n", 1000 * time_spent, acc);
}

