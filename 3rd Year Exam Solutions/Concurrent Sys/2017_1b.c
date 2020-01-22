/**
 * CS3014 Exam 2017 Q 1b
 * 2018-04-25
 * author: Harshvardhan Pandit
 * description: vectorise a nested for loop
 * compile: gcc -o ans.o 2017_1b.c && ./ans.o
 * output:
 * 16777216.000000
 * 16777216.000000
 * notes:
 *   if all values in vector and matrix are equal to 1
 *   then the code is essentially iterating 4096 * 4096 times
 */

#include<stdio.h>
#include<stdlib.h>
#include<xmmintrin.h>

/**
 * normal multiply function
 * prints the sum of matrix + vec operations at the end
 * compare this result with vectorised code to ensure integrity
 */
void multiply(float ** restrict matrix, float * restrict vec, float * restrict result)
{
    float ans = 0.0;
    for(int i = 0;i<4096; i++)
    { 
        float sum = 0.0;
        for ( int j=0; j<4096; j++)
        {
            sum += vec[j] * matrix[i][j];
        }
        result[i] = sum;
        ans += result[i];
    }
    printf("%f\n", ans);
}

/**
 * vectorised multiply function
 * naive approach
 * can be further optimised
 * prints the sum of operations from vectors
 * compare this result with vectorised code to ensure integrity
 */
void multiply_sse(float ** restrict matrix, float * restrict vec, float * restrict result)
{
    float ans = 0.0;
    for(int i = 0;i<4096; i++)
    { 
        // create a sum vector to hold values
        // sum needs a starting value of 0
        __m128 sum = _mm_set1_ps(0.0);
        // __m128 holds 4 floats, therefore increment by 4
        for ( int j=0; j<4096; j=j+4)
        {
            // load the vectors
            __m128 v_j = _mm_load_ps(&vec[j]);
            __m128 v_m = _mm_load_ps(&matrix[i][j]);
            // vec[j] * matrix[i][j];
            __m128 v_r = _mm_mul_ps(v_j, v_m);
            // sum += previous_vector --> vec[j] * matrix[i][j]
            sum = _mm_add_ps(sum, v_r);
        }
        /* also possible to use _mm_hadd_ps and _mm_store_ss if supported */
        // here we add individual elements
        result[i] = sum[0] + sum[1] + sum[2] + sum[3];
        ans += result[i];
    }
    printf("%f\n", ans);
}

int main(int argc, char** argv) {
    float **m = malloc(sizeof(float *)*4096);
    float *v = malloc(sizeof(float)*4096);
    float *r = malloc(sizeof(float)*4096);

    for(int i=0; i<4096; i++) {
        m[i] = (float *) malloc(sizeof(float)*4096);
        for(int j=0; j<4096; j++) {
            m[i][j] = 1;
        }
        v[i] = 1;
    }

    for(int i=0; i<4096; i++, r[i]=0);
    multiply(m, v, r);
    for(int i=0; i<4096; i++, r[i]=0);
    multiply_sse(m, v, r);

    return 0;
}
