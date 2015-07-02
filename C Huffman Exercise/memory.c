#include <stdio.h>
#include <stdlib.h>
#include "memory.h"

/*-----------------------------------------------------------------------------------------*
 * A linked list to record every open memory allocation made using eecs345_malloc or       *
 * eecs345_calloc and a count of the times eecs345_free was used on non-allocated addresses*
 *-----------------------------------------------------------------------------------------*/
struct memlist {
  void * address;
  struct memlist * next;
};

struct memlist * __allocated_memory = NULL;
int __illegal_frees = 0;

/*-----------------------------------------------------------------------------------------*
 * Behaves the same as "normal" malloc, but keeps track of the memory allocation so we can *
 *  watch for memory leaks.                                                                *
 * INPUT:  the number of bytes to be allocated                                             *
 * OUTPUT: a pointer to the allocated memory                                               *
 *-----------------------------------------------------------------------------------------*/
void * eecs345_malloc(int size) {
  void * p = malloc(size);
  struct memlist * memptr = (struct memlist *)malloc(sizeof(struct memlist));
  memptr->address = p;
  memptr->next = __allocated_memory;
  __allocated_memory = memptr;
  return p;
}

/*-----------------------------------------------------------------------------------------*
 * Behaves the same as "normal" calloc, but keeps track of the memory allocation so we can *
 *  watch for memory leaks.                                                                *
 * INPUT:  the number of entries in the desired array and the size in bytes for each entry *
 * OUTPUT: a pointer to the allocated memory                                               *
 *-----------------------------------------------------------------------------------------*/
void * eecs345_calloc(int number, int size) {
  void * p = calloc(number, size);
  struct memlist * memptr = (struct memlist *)malloc(sizeof(struct memlist));
  memptr->address = p;
  memptr->next = __allocated_memory;
  __allocated_memory = memptr;
  return p;
}

/*-----------------------------------------------------------------------------------------*
 * Behaves the same as "normal" free, but does not call free unless the memory was         *
 *   allocated by eecs345_malloc or eecs345_calloc.  Keeps track of the number of times    *
 *   eecs345_free is called incorrectly.                                                   *
 * INPUT:  a pointer to the memory to be deallocated                                       *
 *-----------------------------------------------------------------------------------------*/
void eecs345_free(void * p) {
  if (__allocated_memory == NULL) {
  }
  else if (__allocated_memory->address == p) {
    struct memlist * freeblock = __allocated_memory;
    __allocated_memory = __allocated_memory->next;
    free(p);
    free(freeblock);
    return;
  }
  else {
    struct memlist * memptr = __allocated_memory;
    while (memptr->next != NULL && memptr->next->address != p)
      memptr = memptr->next;
    if (memptr->next != NULL) {
      struct memlist * freeblock = memptr->next;
      memptr->next = memptr->next->next;
      free(freeblock);
      free(p);
      return;
    }
  }
  __illegal_frees++;
}

/*-----------------------------------------------------------------------------------------*
 * Prints a message if there is still allocated memory not freed or if eecs345_free        *
 *   was ever called incorrectly on an address.                                            *
 *-----------------------------------------------------------------------------------------*/
void test_for_memoryleaks() {
  int count = 0;
  struct memlist * memptr = __allocated_memory;
  printf("Checking memory usage......\n");
  while (memptr != NULL) {
    count++;
    memptr = memptr->next;
  }
  if (count == 0)
    printf("\tNo memory leaks!\n");
  else
    printf("\tThere were %d memory allocations that were not freed.\n", count);
  if (__illegal_frees > 0)
    printf("\tThere were %d times free was called on memory that was either not allocated or already freed.\n", __illegal_frees);
}

