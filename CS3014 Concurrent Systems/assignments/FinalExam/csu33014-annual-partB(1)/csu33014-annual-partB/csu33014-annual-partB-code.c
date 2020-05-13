//
// CSU33014 Summer 2020 Additional Assignment
// Part B of a two-part assignment
//
// Please write your solution in this file

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "csu33014-annual-partB-person.h"

void find_reachable_recursive(struct person * current, int steps_remaining,
			      bool * reachable) {
  // mark current root person as reachable
  reachable[person_get_index(current)] = true;
  // now deal with this person's acquaintances
  if ( steps_remaining > 0 ) {
    int num_known = person_get_num_known(current);
    for ( int i = 0; i < num_known; i++ ) {
      struct person * acquaintance = person_get_acquaintance(current, i);
      find_reachable_recursive(acquaintance, steps_remaining-1, reachable);
    }
  }
}

// computes the number of people within k degrees of the start person
int number_within_k_degrees(struct person * start, int total_people, int k) {
  bool * reachable;
  int count;

  // maintain a boolean flag for each person indicating if they are visited
  reachable = malloc(sizeof(bool)*total_people);
  for ( int i = 0; i < total_people; i++ ) {
    reachable[i] = false;
  }

  // now search for all people who are reachable with k steps
  find_reachable_recursive(start, k, reachable);

  // all visited people are marked reachable, so count them
  count = 0;
  for ( int i = 0; i < total_people; i++ ) {
    if ( reachable[i] == true ) {
      count++;
    }
  }
  return count;
}

struct QNode {
    struct person * data;
    int distance;
    struct QNode* next;
};

// The queue, front stores the front node of LL and rear stores the
// last node of LL
struct Queue {
    struct QNode *front, *rear;
};

// A utility function to create a new linked list node.
struct QNode* newNode(struct person * data,int distance)
{
    struct QNode* temp = (struct QNode*)malloc(sizeof(struct QNode));
    temp->data = data;
    temp->distance = distance;
    temp->next = NULL;
    return temp;
}

// A utility function to create an empty queue
struct Queue* createQueue()
{
    struct Queue* q = (struct Queue*)malloc(sizeof(struct Queue));
    q->front = q->rear = NULL;
    return q;
}

// The function to add a key k to q
void enQueue(struct Queue* q, struct person * data, int dist)
{
    // Create a new LL node
    struct QNode* temp = newNode(data,dist);

    // If queue is empty, then new node is front and rear both
    if (q->rear == NULL) {
        q->front = q->rear = temp;
        return;
    }

    // Add the new node at the end of queue and change rear
    q->rear->next = temp;
    q->rear = temp;
}

// Function to remove a key from given queue q
struct QNode * deQueue(struct Queue* q)
{
    // If queue is empty, return NULL.
    if (q->front == NULL)
        return NULL;

    // Store previous front and move front one node ahead
    struct QNode* temp = q->front;

    q->front = q->front->next;

    // If front becomes NULL, then change rear also as NULL
    if (q->front == NULL)
        q->rear = NULL;

    struct QNode * retNode =  newNode(temp->data, temp->distance);
    free(temp);
    return(retNode);
}

bool isEmpty(struct Queue * q){
    if(q->front == NULL){
        return 0;
    }else{
        return 1;
    }
}

// computes the number of people within k degrees of the start person;
// less repeated computation than the simple original version
int less_redundant_number_within_k_degrees(struct person * start,int total_people, int k) {
    bool * reachable;
    reachable = malloc(sizeof(bool)*total_people);
    for ( int i = 0; i < total_people; i++ ) {
        reachable[i] = false;
    }
  return find_reachable_bfs(start,k,reachable,total_people);
}

int find_reachable_bfs(struct person * current,int steps_remaining, bool * reachable,int totalppl){
    struct Queue * q = createQueue();
    int total = 0;
    enQueue(q,current,0);
    while( isEmpty(q) == 1){
        struct QNode * curr = deQueue(q);
        int numKnown  = person_get_num_known(curr->data);
        if(curr->distance <= steps_remaining && reachable[person_get_index(curr->data)] == false){
            for(int i = 0; i<numKnown; i++){
                struct person * next  = person_get_acquaintance(curr->data,i);
                if(reachable[person_get_index(next)] == false){
                    enQueue(q,next,curr->distance+1);
                }
            }
            reachable[person_get_index(curr->data)] = true;
            total++;
        }
    }
    return(total);
}

// computes the number of people within k degrees of the start person;
// parallel version of the code
int parallel_number_within_k_degrees(struct person * start,
				     int total_people, int k) {
  return number_within_k_degrees(start, total_people, k);
}







