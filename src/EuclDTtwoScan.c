#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// **************************
//
// This algorithm is a implementation of the paper 
//    Fast Euclidean distance transformation
//    in two scans using a 3 ␁ 3 neighborhood
// by Frank Y. Shih* and Yi-Ta Wu
//
//
// **************************
void initialisation(int* binary_image, int *rx,int*ry,int cols, int rows, int * output) {

   for (int i=0; i<rows; i++) {
      for (int j=0; j<cols; j++) {
         if (*(binary_image+i * cols+j) == 1) {
            *(output + i * cols +j) = INFINITY;
            *(rx + i * cols +j)=0;
            *(ry + i * cols +j)=0;
         }
      }
   }
}


// **************************
//
// calculates the h(p,q) function out of the paper
//
// **************************
//
// p stellt den akutellen punkt in der matrix dar und q
// ist der jetzige abzuprüfenden punkt, der um p liegt und 
// geupdated werden muss 
// p = (i,j) und q=(ni,nj)
// rx und ry sind update Matrizen die die Eentferung tracken
// max_cols is only there to calculate the index of the output matrix
int h(int i, int j, int ni, int nj, int * rx, int * ry, int max_cols){
   if (i=ni){
      
      return 2 * *(rx+ ni * max_cols + nj) + 1;
   } else if (j==nj) {
      
      return 2 * *(rx+ ni * max_cols + nj) + 1; 
   } else {
      return 2 * (*(rx+ ni * max_cols + nj) + *(ry+ ni * max_cols + nj) + 1);
   }
   return -1;
}

void update_distance(int* rx, int* ry, int* output, int i, int j, int ni, int nj, int max_cols) {

   int new_output_value = *(output + ni * max_cols + nj) + h(i,j,ni,nj,rx,ry,max_cols);
   if (new_output_value < *(output + i * max_cols + j)){
      *(rx+ i * max_cols + j) = *(rx+ ni * max_cols + nj) + abs(i - ni);
      *(ry+ i * max_cols + j) = *(ry+ ni * max_cols + nj) + abs(j - nj);
      *(output + i * max_cols + j) = new_output_value;
   }

}

void forward_scan(int * binary_image,int* rx, int* ry, int rows, int cols, int* output) {
   for (int i=0; i<rows; i++){
      for (int j=0; j<cols; j++){
         *(output+i*cols*j)=1;
         if (binary_image[i * cols+j]==1){
            if (i>0)  update_distance(rx, ry, output, i, j, i-1, j, cols);
            if (j>0)  update_distance(rx, ry, output, i, j, i, j-1, cols);
            if (i>0 && j >0) update_distance(rx, ry, output, i, j, i-1, j-1, cols);
            if (i>0 && j < cols-1) update_distance(rx,ry,output,i,j,i-1, j+1, cols);
         }
      }
   }
}

void backward_scan();

void EuclideanDistanceTransformation(int * binary_image, int *rows, int *cols, int * output){
   // initialization
   int *rx = malloc(sizeof(int) * (*rows + *cols));
   int *ry = malloc(sizeof(int) * (*rows + *cols));
   initialisation(binary_image, rx, ry, *cols, *rows, output);

   for (int i=0;i<*rows * *cols;i++){
      printf("%d",*(binary_image + i));
   }
   printf("------------------\n");
   for (int i=0;i<*rows * *cols;i++){
      printf("%d",*(output + i));
   }
   printf("------------------\n");


   // forward raster scan
   forward_scan(binary_image, rx, ry, *cols, *rows, output);
   free(rx);
   free(ry);

}

int main(){
   int a = 4;
   int b = 3;
   int * p = &a;
   int * r = &b;
   EuclideanDistanceTransformation(p, r, p, p);

return 0;

}
