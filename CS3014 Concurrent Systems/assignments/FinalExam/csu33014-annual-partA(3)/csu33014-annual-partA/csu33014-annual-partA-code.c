//
// CSU33014 Summer 2020 Additional Assignment
// Part A of a two-part assignment
//

// Please examine version each of the following routines with names
// starting partA. Where the routine can be vectorized, please
// complete the corresponding vectorized routine using SSE vector
// intrinsics. Where is cannot be vectorized...

// Note the restrict qualifier in C indicates that "only the pointer
// itself or a value directly derived from it (such as pointer + 1)
// will be used to access the object to which it points".


#include <immintrin.h>
#include <stdio.h>

#include "csu33014-annual-partA-code.h"

/****************  routine 0 *******************/

// Here is an example routine that should be vectorized
void partA_routine0(float * restrict a, float * restrict b,
		    float * restrict c) {
  for (int i = 0; i < 1024; i++ ) {
    a[i] = b[i] * c[i];
  }
}

// here is a vectorized solution for the example above
void partA_vectorized0(float * restrict a, float * restrict b,
		    float * restrict c) {
  __m128 a4, b4, c4;
  
  for (int i = 0; i < 1024; i = i+4 ) {
    b4 = _mm_loadu_ps(&b[i]);
    c4 = _mm_loadu_ps(&c[i]);
    a4 = _mm_mul_ps(b4, c4);
    _mm_storeu_ps(&a[i], a4);
  }
}

/***************** routine 1 *********************/

// in the following, size can have any positive value
float partA_routine1(float * restrict a, float * restrict b,
		     int size) {
  float sum = 0.0;
  
  for ( int i = 0; i < size; i++ ) {
    sum = sum + a[i] * b[i];
  }
  return sum;
}

// insert vectorized code for routine1 here
float partA_vectorized1(float * restrict a, float * restrict b,
		     int size) {
  // replace the following code with vectorized code
  float sum = 0.0;
  if(size%4 == 0){
    float xx[4];
    __m128 *av = (__m128*)a; // assume 16 byte aligned
    __m128 *bv = (__m128*)b; // assume 16-byte aligned
    __m128 xv = _mm_setzero_ps();
    for (int i = 0; i < size/4; i++)
        xv = _mm_add_ps(xv, _mm_mul_ps(av[i], bv[i]));
    _mm_store_ps(xx, xv);
    sum = xx[0] + xx[1] + xx[2] + xx[3];
  }
  else{
    for ( int i = 0; i < size; i++ ) {
      sum = sum + a[i] * b[i];
    }
  }
  
  return sum;
}

/******************* routine 2 ***********************/

// in the following, size can have any positive value
void partA_routine2(float * restrict a, float * restrict b, int size) {
  for ( int i = 0; i < size; i++ ) {
    a[i] = 1 - (1.0/(b[i]+1.0));
  }
}

// in the following, size can have any positive value
void partA_vectorized2(float * restrict a, float * restrict b, int size) {
  // replace the following code with vectorized code
  __m128 b_vals,ones;
  if(size%4 == 0){
    ones = _mm_set1_ps(1.0);
    for(int i=0;i<size;i=i+4){
      b_vals = _mm_loadu_ps(&b[i]);
      b_vals = _mm_add_ps(b_vals,ones);
      b_vals = _mm_div_ps(ones,b_vals);
      b_vals = _mm_sub_ps(ones,b_vals);
      _mm_storeu_ps(&a[i],b_vals);
    }
  }
  else
  {
     for ( int i = 0; i < size; i++ ) {
        a[i] = 1 - (1.0/(b[i]+1.0));
  }
  }
  
}

/******************** routine 3 ************************/

// in the following, size can have any positive value
void partA_routine3(float * restrict a, float * restrict b, int size) {
  for ( int i = 0; i < size; i++ ) {
    if ( a[i] < 0.0 ) {
      a[i] = b[i];
    }
  }
}

// in the following, size can have any positive value
void partA_vectorized3(float * restrict a, float * restrict b, int size) {
  // replace the following code with vectorized code
  if(size%4 == 0){
      for(int i =0; i<size;i=i+4){

              __m128 a_vals,b_vals,zeroes,mask,mask2;
              for(int i =0;i<size;i=i+4){
                  a_vals = _mm_loadu_ps(&a[i]);
                  b_vals = _mm_loadu_ps(&b[i]);
                  zeroes = _mm_set1_ps(0.0);
                  mask = _mm_cmple_ps(zeroes,a_vals);
                  mask2 = _mm_cmpnle_ps(zeroes,a_vals);
                  a_vals = _mm_and_ps(a_vals,mask);
                  b_vals = _mm_and_ps(b_vals,mask2);
                  a_vals = _mm_add_ps(a_vals,b_vals);
                  _mm_storeu_ps(&a[i],a_vals);
              }
      }
  }else{
      for ( int i = 0; i < size; i++ ) {
          if ( a[i] < 0.0 ) {
              a[i] = b[i];
          }
      }
  }
}

/********************* routine 4 ***********************/

// hint: one way to vectorize the following code might use
// vector shuffle operations
void partA_routine4(float * restrict a, float * restrict b,
		       float * restrict c) {
  for ( int i = 0; i < 2048; i = i+2  ) {
    a[i] = b[i]*c[i] - b[i+1]*c[i+1];
    a[i+1] = b[i]*c[i+1] + b[i+1]*c[i];
  }
}

void partA_vectorized4(float * restrict a, float * restrict b,
		       float * restrict  c) {
  // replace the following code with vectorized code
  __m128 b_val,c_val,b_shuf,c_shuf,a1,a2,ans,inter,inter2;
  for ( int i = 0; i < 2048; i = i+4  ) {
    b_val =_mm_loadu_ps(&b[i]);
    c_val = _mm_loadu_ps(&c[i]);
    b_shuf = _mm_shuffle_ps(b_val,b_val,_MM_SHUFFLE(0,1,3,2));
    b_shuf = _mm_shuffle_ps(b_val,b_shuf,_MM_SHUFFLE(0,1,0,1));
    c_shuf = _mm_shuffle_ps(c_val,c_val,_MM_SHUFFLE(0,1,3,2));
    c_shuf = _mm_shuffle_ps(c_val,c_shuf,_MM_SHUFFLE(0,1,0,1));
    a1= _mm_mul_ps(b_val,c_val);
    a2 = _mm_mul_ps(b_val,c_shuf);
    inter = _mm_mul_ps(b_shuf,c_shuf);
    inter2 = _mm_mul_ps(b_shuf,c_val);
    a2 = _mm_add_ps(a2,inter2);
    a1 = _mm_sub_ps(a1,inter);


      //inter = _mm_mul_ps(b_shuf,c_shuf);
     // inter2 = _mm_mul_ps(b_shuf,c_val);
    ans = _mm_shuffle_ps(a1,a2,_MM_SHUFFLE(2,0,2,0));
    ans = _mm_shuffle_ps(ans,ans,_MM_SHUFFLE(3,1,2,0));
    _mm_storeu_ps(&a[i],ans);
  }
}

/********************* routine 5 ***********************/

// in the following, size can have any positive value
void partA_routine5(unsigned char * restrict a,
		    unsigned char * restrict b, int size) {
  for ( int i = 0; i < size; i++ ) {
    a[i] = b[i];
  }
}

void partA_vectorized5(unsigned char * restrict a,
		       unsigned char * restrict b, int size) {
  // replace the following code with vectorized code
  __m128i* a_vals;
  __m128i* b_vals;
  if(size % 16 == 0){
    a_vals = (__m128i*)a;
    b_vals = (__m128i*)b;
    __m128i zeroes = _mm_set_epi32(0,0,0,0);
    for(int i =0;i<size/16;i++){
      __m128i source = _mm_loadu_si128(&b_vals[i]);
      __m128i val = _mm_add_epi8(source,zeroes);
      _mm_storeu_si128(&a_vals[i],val);
    }
  }else{
     for ( int i = 0; i < size; i++ ) {
    a[i] = b[i];
  }
  }
}

/********************* routine 6 ***********************/

void partA_routine6(float * restrict a, float * restrict b,
		       float * restrict c) {
  a[0] = 0.0;
  for ( int i = 1; i < 1023; i++ ) {
    float sum = 0.0;
    for ( int j = 0; j < 3; j++ ) {
      sum = sum +  b[i+j-1] * c[j];
    }
    a[i] = sum;
  }
  a[1023] = 0.0;
}

void partA_vectorized6(float * restrict a, float * restrict b,
		       float * restrict c) {
  // replace the following code with vectorized code
  a[0] = 0.0;
  for ( int i = 1; i < 1023; i++ ) {
    float sum = 0.0;
    for ( int j = 0; j < 3; j++ ) {
      sum = sum +  b[i+j-1] * c[j];
    }
    a[i] = sum;
  }
  a[1023] = 0.0;
}



