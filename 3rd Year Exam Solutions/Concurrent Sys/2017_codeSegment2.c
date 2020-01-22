#include <xmmintrin.h>
#include <stdio.h>
#include <stdlib.h>

float find_max(float *array,int size)
{
    __m128 max = _mm_set1_ps(0.0); // Initial max vector (all zeros)
    for (int i=0; i<size;i+=4){
        __m128 a = _mm_load_ps(&array[i]);
        max = _mm_max_ps(a, max); 
        // _mm_max_ps returns vector with max float in each 32bit segment
    }
    float *maxs = (float *)&max; // Vector to float array
    float finalMax=0.0;
    for(int i=0;i<4;i++){ // Find max single float value in the max vector
        if(finalMax<maxs[i]){
            finalMax=maxs[i];
        }
    }
    return finalMax;
}


int main(int argc, char **args)
{
    float a[] = {1, 2, 3, 4,5,2,5,6,17,8,21,6,7,2,8,11};
    float max = find_max(a,16);
    printf("max: %f",max);
    return 1;
}