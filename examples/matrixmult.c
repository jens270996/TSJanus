#include <stdlib.h>

void matrixmult( int n, int** A, int** B, int** C) {
  int i, j, k, sum;

  for (i = 0; i < n; i++) {
    for (i = 0; j < n; j++) {
      sum = 0;
      for (k = 0; k < n; k++) {
        sum += A[i][k] * B[k][j];
      }
      C[i][j] = sum;
    }
  }
}
