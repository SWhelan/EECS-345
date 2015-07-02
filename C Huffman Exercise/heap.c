//Sarah Whelan
//slw96
//3/26/2015

#include <stdio.h>
#include <stdlib.h>
#include "heap.h"

#define HEAP_ROOT	1    /* the root is at index 1 of the heap array; index 0 is unused */

void bubbleup(struct heap *, int);
void bubbledown(struct heap *, int);

/*--------------------------------------------------------------------------------------*
 * Compare functions for the heap sort method                                           *
 *--------------------------------------------------------------------------------------*/ 
int (*normalCompare)(void *, void*);
 
int reverseCompare(void * a, void * b){
    return normalCompare(a, b) * -1;
}

/*--------------------------------------------------------------------------------------*
 * Does an in-place heap sort of the array.                                             *
 * INPUT:  The array to be sorted, the size of the array, and a function to order the   *
 *         elements.  The array stores pointers to the elements that must be sorted.    *
 *--------------------------------------------------------------------------------------*/ 
 void heapSort(void ** data, int size, int (*compare)(void *, void *)) {
  int i;

  normalCompare = compare;
  struct heap heap;
  //this is allowed because the index 0 of the heap is never accessed and this effectively makes the heap the data array 
  heap.data = data - 1;
  heap.capacity = size;
  heap.endptr = size;
  heap.compare = reverseCompare;
  
  /*enforce the heap constraint (each parent is as large or larger than both its children) by
  calling bubble down on all nodes that would have children */
  for(i = heap.capacity / 2; i > 0; i--){
    bubbledown(&heap, i);
  }
  
  /* takes the value at the top of the heap and puts it last in the data */
  while(!heap_empty(&heap)){
   data[heap.endptr] = heap_remove_min(&heap);
  }
}

/*--------------------------------------------------------------------------------------*
 * Creates an empty heap and returns a pointer to the heap                              *
 * OUTPUT: A pointer to the heap.                                                       *
 *--------------------------------------------------------------------------------------*/
struct heap * initialize_heap(int capacity, int (* compare)(void *, void *)) {
  struct heap * newheap = (struct heap *)malloc(sizeof(struct heap));
  newheap->capacity = capacity;
  newheap->data = (void *)calloc(newheap->capacity + 1, sizeof(void *));    /* index 0 is not used in the heap */
  newheap->endptr = 0;
  newheap->compare = compare;
  return newheap;
}

/*--------------------------------------------------------------------------------------*
 * Frees the memory used by the heap and the heap pointer                               *
 * INPUT:  A pointer to the heap.                                                       *
 *--------------------------------------------------------------------------------------*/
void free_heap(struct heap * heap) {
  free((void *)heap->data);
  free((void *)heap);
}

/*--------------------------------------------------------------------------------------*
 * Returns the element, but does not remove, the element at the top of the heap.        *
 * INPUT:  A pointer to the heap.                                                       *
 * OUTPUT: The minimum element.                                                         *
 *--------------------------------------------------------------------------------------*/
void * heap_get_min(struct heap * heap) {
  if (heap->endptr < HEAP_ROOT)                             /* if the heap is empty .... */
    return NULL;
  else                                                      /* otherwise, get the root element */
    return heap->data[HEAP_ROOT];
}

/*--------------------------------------------------------------------------------------*
 * Removes and returns the element at the top of the heap.                              *
 * INPUT:  A pointer to the heap.                                                       *
 * OUTPUT: A pointer to the minimum element.                                            *
 *--------------------------------------------------------------------------------------*/
void * heap_remove_min(struct heap * heap) {
  if (heap->endptr < HEAP_ROOT)                             /* if the heap is empty .... */
    return NULL;      
  else {                                                    /* otherwise, we: */
    void * min_element = heap->data[HEAP_ROOT];             /* remove the root element */
    heap->data[1] = heap->data[heap->endptr--];             /* move the last leaf element to the root */
    bubbledown(heap, 1);                                    /* and bubble it down */
    return min_element;
  }
}

/*--------------------------------------------------------------------------------------*
 * Adds another element to the heap.                                                    *
 * INPUT:  A pointer to the heap, and a pointer to the data.                            *
 *--------------------------------------------------------------------------------------*/
void add_to_heap(struct heap * heap, void * data) {
  heap->data[++heap->endptr] = data;                       /* place the new element at the last leaf */
  bubbleup(heap, heap->endptr);                            /* and bubble it up */
}

/*--------------------------------------------------------------------------------------*
 * Returns true if the heap is empty.                                                   *
 * INPUT:  A pointer to the heap.                                                       *
 * OUTPUT: 0 if there are no elements in the heap and non-0 otherwise.                  *
 *--------------------------------------------------------------------------------------*/
char heap_empty(struct heap * heap) {
  return heap->endptr == 0;
}

/*--------------------------------------------------------------------------------------*
 * bubbleup takes a heap and the index of a node in the heap.  The method assumes the   *
 *   element at that node is "light" and moves it up the heap until the node has a      *
 *   parent that is smaller than it is.                                                 *
 *--------------------------------------------------------------------------------------*/
void bubbleup(struct heap * heap, int index) {
  int parent = index / 2;                                              /* the index of the parent is half the index of the node */
  void * temp;

  if (index == HEAP_ROOT)                                              /* if we are at the root, we can't go higher */
    return;

  if (heap->compare(heap->data[index], heap->data[parent]) < 0) {      /* if node is smaller than its parent */
    temp = heap->data[index];                                          /* swap the node with the parent */
    heap->data[index] = heap->data[parent];
    heap->data[parent] = temp;
    bubbleup(heap, parent);                                            /* and repeat */
  }
}

/*--------------------------------------------------------------------------------------*
 * bubbledown takes a heap and the index of a node in the heap.  The method assumes the *
 *   element at that node is "heavy" and moves it down the heap until the node has both *
 *   children larger than it is.                                                        *
 *--------------------------------------------------------------------------------------*/
void bubbledown(struct heap * heap, int index) {
  int leftchild = index * 2;                                          /* the left child index is twice the node's index */
  int rightchild = leftchild + 1;                                     /* and the right child's index is one more */
  int child;
  void * temp;

  if (leftchild > heap->endptr)                                       /* if we are at a leaf, we can't go lower */
    return;

  if (rightchild > heap->endptr)                                      /* otherwise, child will store the smaller of the two children */
    child = leftchild;                                                 
  else if (heap->compare(heap->data[leftchild], heap->data[rightchild]) < 0)
    child = leftchild;
  else
    child = rightchild;

  if (heap->compare(heap->data[child], heap->data[index]) < 0) {     /* if the node is larger than its smallest child */
    temp = heap->data[index];                                        /* swap the node with its smaller child */
    heap->data[index] = heap->data[child];
    heap->data[child] = temp;
    bubbledown(heap, child);                                         /* and repeat */
  }
}