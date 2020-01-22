// ConsoleApplication4.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>

#define s_size 3
#define a_size 2
#define y 0.1

float q0(int, int);
float r(int, int,int);
float p(int, int, int);
float qn(int, int, int);
float Vn(int, int);

float arr[a_size][s_size][s_size][2] =
{
	{ 
	
		{{.5,3},{.3,0},{.2,-2}},
		{{.3,0},{.5,1},{.2,2}},
		{{0,0},{0,0},{1,1}},
		
	},
	{
		{{.2,4},{.2,2},{.6,-3}},
		{{.1,1},{0,0},{.9,-2}},
		{{0,0},{0,0},{1,0}},
	}
};

int main()
{
	float n = 2;
	float s3 = 2;
	float a2 = 1;
	float p = qn(n, s3, a2);
    std::cout << "Hello World!\n" << p; 
}

float q0(int s, int a)
{
	float total = 0.0f;

	for (int i = 0; i < s_size; i++)
		total = total + (p(s,a,i) * r(s,a,i));
	return total;
}

float Vn(int n, int s)
{
	float max = qn(n, s, 0);

	for (int i = 1; i < s_size; i++)
	{
		float newVal = qn(n, s, i);

		if (newVal > max)
			max = newVal;
	}

	return max;
}

float qn(int n, int s, int a)
{
	if (n == 0)
		return q0(s, a);
	else
	{
		float discount_total = 0.0;
		for (int i = 0; i < s_size; i++)
			discount_total = discount_total + (p(s, a, i)*Vn(n - 1, i));

		return q0(s, a) + y * discount_total;
	}
}

float p(int s, int a, int sN)
{
	return arr[a][s][sN][0];
}

float r(int s, int a, int sN)
{
	return arr[a][s][sN][1];
}

