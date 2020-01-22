// view with syntax highlighting - https://gist.github.com/RoryDH/9988f485ef36fd7a653fe79b54bfd046

#include <stdio.h>
#include <stdlib.h>
#include <xmmintrin.h>

#define SIZE 600

float rgb_sum_product(float * pixels) {
  float sum_red = 0.0, sum_green = 0.0, sum_blue = 0.0;
  for (int i = 0; i < SIZE - 3; i += 3) {
    sum_red   = sum_red +   pixels[i];
    sum_green = sum_green + pixels[i+1];
    sum_blue  = sum_blue +  pixels[i+2];
  }
  return sum_red * sum_green * sum_blue;
}

float rgb_sum_product_vec(float * pixels) {
  float *sums = malloc(sizeof(float) * 4); // make space for final result after adding vectors

  __m128 vsums = _mm_set1_ps(0.0f); // create a vector for the sum of each value, starting at 0

  for (int i = 0; i < SIZE - 3; i += 3) {
    // assign the rgb values to a new vector
    // __m128 v = _mm_loadu_ps(&pixels[i]);
    //  could do unaligned loadu like above but it's "4 * slower" supposedly anyway and we'll also
    //   need to make sure the last iteration doesn't load from out of bounds
    __m128 v = _mm_setr_ps(pixels[i], pixels[i+1], pixels[i+2], 0.0);
    vsums = _mm_add_ps(vsums, v); // add the vector to the sum
  }

  _mm_store_ps(sums, vsums); // store the vector in the standard float array

  return sums[0] * sums[1] * sums[2]; // return the product
}

int main(void)
{
  float * pixels = malloc(sizeof(float) * SIZE);
  for (int i = 0; i < SIZE; ++i)
  {
    pixels[i] = i*1.0;
  }


  printf("%f\n", rgb_sum_product(pixels));
  printf("%f\n", rgb_sum_product_vec(pixels));

  int u2 = 8;
  int u = 8;

  printf("%p\n", pixels);
  printf("%p %d\n", &u, ((unsigned long)(&u2) & 15) == 0);

  return 0;
}
