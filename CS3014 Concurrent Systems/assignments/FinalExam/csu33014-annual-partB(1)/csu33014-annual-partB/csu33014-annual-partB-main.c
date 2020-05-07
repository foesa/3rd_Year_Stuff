//
// CSU33014 Summer 2020 Additional Assignment
// Part B of a two-part assignment
//
// Main file for testing the parallel routine
//
// Please do not change anything in this file

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <sys/time.h>
#include "csu33014-annual-partB-person.h"
#include "csu33014-annual-partB-code.h"

// generate a random integer, which may or may not be signed
int get_random(int range, int is_signed) {
  int num = random();
  int sign = random();

  num = num % range;
  // if signed, then 50% should be negative
  if ( is_signed && (sign % 2 == 1) ) {
    num = -num;
  }
  return num;
}

// generate a random graph of people
struct person ** gen_random_people_graph(int nnodes, int nedges) {
  struct person ** nodes;

  // create the nodes
  nodes = malloc(sizeof(struct person*) * nnodes);
  for ( int i = 0; i < nnodes; i++ ) {
    nodes[i] = person_new(i);
  }

  // create the edges
  for ( int i = 0; i < nedges; i++ ) {
    bool success = false;
    while ( success == false ) {
      // generate the indices of two nodes to be connected by an edge
      int node1 = get_random(nnodes, 0);
      int node2 = get_random(nnodes, 0);
      // don't try to connect a node to itself
      if ( node1 != node2 ) {
	// don't try to connect nodes that are already connected
	if ( !person_is_connected(nodes[node1], nodes[node2]) ) {
	  // success, we have two distinct nodes not already connected
	  person_add_connection(nodes[node1], nodes[node2]);
	  person_add_connection(nodes[node2], nodes[node1]);
	  success = true;
	}
      }
    }
  }
  return nodes;
}

// seed the pseudo-random number generator using the system clock
void initialize_random() {
  struct timeval seedtime;
  int seed;
  
  /* use the microsecond part of the current time as a pseudorandom seed */
  gettimeofday(&seedtime, NULL);
  seed = seedtime.tv_usec;
  srandom(seed);
}

// get the difference in microseconds between two times
long long time_diff(struct timeval start, struct timeval stop) {
  long long result = (stop.tv_sec - start.tv_sec) * 1000000L +
    (stop.tv_usec - start.tv_usec);
  return result;
}

// main function to test the code
int main(int argc, char ** argv) {
  struct person ** graph;
  int count_warmup, count_correct, count_reduced, count_parallel;
  struct timeval start_time;
  struct timeval stop_time;
  long long compute_time;

  if ( argc != 3 ) {
    fprintf(stderr, "Usage: ./graph <nodes> <k>\n");
    exit(1);
  }

  int nnodes = atoi(argv[1]);
  int k = atoi(argv[2]);
  
  // initialize the pseudo-random number generator
  initialize_random();

  // create a random graph of people
  const int nedges = nnodes * 2;
  graph = gen_random_people_graph(nnodes, nedges);

  // compute the function once to warm up the processor's caches
  const bool warmup = false;
  if ( warmup ) {
    gettimeofday(&start_time, NULL);
    count_warmup = number_within_k_degrees(graph[0], nnodes, k);
    gettimeofday(&stop_time, NULL);
    compute_time = time_diff(start_time, stop_time);
    fprintf(stderr, "Warm up count %d, Time: %lld\n", count_warmup, compute_time);
  }
  
  
  // count the number of people reachable from node zero within k steps
  gettimeofday(&start_time, NULL);
  count_correct = number_within_k_degrees(graph[0], nnodes, k);
  gettimeofday(&stop_time, NULL);
  compute_time = time_diff(start_time, stop_time);
  fprintf(stderr, "Correct count %d, Time: %lld\n", count_correct, compute_time);

  // check the student's version that reduces redundant work
  gettimeofday(&start_time, NULL);
  count_reduced = less_redundant_number_within_k_degrees(graph[0], nnodes, k);
  gettimeofday(&stop_time, NULL);
  compute_time = time_diff(start_time, stop_time);
  fprintf(stderr, "Less redundant count %d, Time: %lld\n", count_reduced, compute_time);
  
  // check the student's parallel version
  gettimeofday(&start_time, NULL);
  count_parallel = parallel_number_within_k_degrees(graph[0], nnodes, k);
  gettimeofday(&stop_time, NULL);
  compute_time = time_diff(start_time, stop_time);
  fprintf(stderr, "Parallel count %d, Time: %lld\n", count_parallel, compute_time);
  
  return 0;
}
