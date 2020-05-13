#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <immintrin.h>
int main() {
    float a[1023];
    float c[]= {1.0f,2.0f,3.0f,0.0f,5.0f,6.0f,7.0f,-1.0f};
    float b[] = {2.0f,3.0f,4.0f,5.0f,6.0f,7.0f,8.0f,9.0f};
    __m128 a_vals,b_vals,zeroes,mask,mask2;
    for(int i =0; i<8;i=i+4){
            for(int i =0;i<1023;i=i+4){
                a_vals = _mm_loadu_ps(&c[i]);
                b_vals = _mm_loadu_ps(&b[i]);
            }
        }
// or instead of the two lines above we can use a horizontal add:
    
}
