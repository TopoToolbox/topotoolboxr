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
            *(output + i * cols +j) = INT32_MAX;
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
   if (i==ni){
      return 2 * *(rx+ ni * max_cols + nj) + 1;
   } else if (j==nj) {
      
      return 2 * *(rx+ ni * max_cols + nj) + 1; 
   }
   return 2 * (*(rx+ ni * max_cols + nj) + *(ry+ ni * max_cols + nj) + 1);
}

void update_distance(int* rx, int* ry, int* output, int i, int j, int ni, int nj, int max_cols) {
   int new_output_value = *(output + ni * max_cols + nj) + h(i,j,ni,nj,rx,ry,max_cols); 
   if (new_output_value < *(output + i * max_cols + j)){
      *(rx+ i * max_cols + j) = *(rx+ ni * max_cols + nj) + abs(i - ni);
      *(ry+ i * max_cols + j) = *(ry+ ni * max_cols + nj) + abs(j - nj);
      *(output + i * max_cols + j) = new_output_value;
   }
}

void forward_scan(int * binary_image,int* rx, int* ry, int cols, int rows, int* output) {
   for (int i=0; i<rows; i++){
      for (int j=0; j<cols; j++){
         if (binary_image[i * cols+j]==1){
            if (i>0){update_distance(rx, ry, output, i, j, i-1, j, cols);
            }
            if (j>0){update_distance(rx, ry, output, i, j, i, j-1, cols);
            }
            if (i>0 && j >0){update_distance(rx, ry, output, i, j, i-1, j-1, cols);
            }
            if (i>0 && j < cols-1){update_distance(rx,ry,output,i,j,i-1, j+1, cols);
            }
         }
      }
   }
}

void backward_scan(int * binary_image, int * rx, int* ry, int cols, int rows, int* output){
   for (int i=rows-1; i>=0; i--){
      for (int j=cols-1; j>=0; j--){
         if (binary_image[i * cols+j]==1){
            if (i<rows-1 && j>0){update_distance(rx, ry, output, i, j, i+1, j-1, cols);
            }
            if (i< rows-1){update_distance(rx, ry, output, i, j, i+1, j, cols);
            }
            if (i<rows-1 && j<cols-1){update_distance(rx, ry, output, i, j, i+1, j+1, cols);
            }
            if (j<cols-1){update_distance(rx,ry,output,i,j,i, j+1, cols);
            }
         }
      }
   }
}

void EuclideanDistanceTransformation(int * binary_image, int *rows, int *cols, int * output, int * rx, int *ry){
   // initialisation
   initialisation(binary_image, rx, ry, *cols, *rows, output);

   // forward raster scan
   forward_scan(binary_image, rx, ry, *cols, *rows, output);
   
   /*for (int i=0; i<*rows* *cols; i++){
      //*(output+i)=0;
      printf("%d",*(output+i));
   }
   printf("%d\n",INT32_MAX);
   */

   backward_scan(binary_image, rx, ry, *cols, *rows, output);

   //free(rx);
   //free(ry);

}
