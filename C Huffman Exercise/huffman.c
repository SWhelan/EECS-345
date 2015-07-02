//Sarah Whelan
//slw96
//3/26/2015

/* A program to read in an input file and produce the Huffman codes that are used to compress the file. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "heap.h"

/* This is the node of the huffman tree */

struct hufftreenode {
  int value;                            // the character being stored
  int weight;                           // the weight is the number of occurrences of the character in the file
  struct hufftreenode * leftchild;
  struct hufftreenode * rightchild;
};

/* A list of Huffman codes for each of the 256 legal C characters */
char * huffmancodes[256];                     

/* a method to create a hufftreenode with initial values */
struct hufftreenode * initialize_node(int value, int weight){
    struct hufftreenode * newnode = (struct hufftreenode *)eecs345_malloc(sizeof(struct hufftreenode));
    newnode->value = value;
    newnode->weight = weight;
    newnode->leftchild = NULL;
    newnode->rightchild = NULL;
    return newnode;
}

/* compares the weight of a hufftreenode*/
int huffmanNodeCompare(struct hufftreenode *a, struct hufftreenode *b){
    return a->weight - b->weight;
}

/* traverses the huffman tree creating and saving the codes for each node */
void traverse(struct hufftreenode * root, char * code){
    int i;
    
    //if the node is a leaf node save the code
    if(root->value != -1){
        huffmancodes[root->value] = code;
        eecs345_free(root);
        return;
    }
    
    //if the node has a left child recursively traverse the left tree
    if(root->leftchild){
        //update the code indicating that the traversal took a left turn here
        char * newcode = (char *)eecs345_malloc((strlen(code) + 2) * sizeof(char));
        for(i = 0; i < strlen(code)+2; i++){
            newcode[i] = code[i];
        }
        newcode[strlen(newcode)] = '0';
        newcode[strlen(newcode) + 1] = '\0';
        
        traverse(root->leftchild, newcode);
    }
    
    //if the node has a right child recursively traverse the right tree
    if(root->rightchild){
        //update the code indicating that the traversal took a right turn here
        char * newcoderight = (char *)eecs345_malloc((strlen(code) + 2) * sizeof(char));
        for(i = 0; i < strlen(code)+2; i++){
            newcoderight[i] = code[i];
        }
        newcoderight[strlen(newcoderight)] = '1';
        newcoderight[strlen(newcoderight) + 1] = '\0';
        
        traverse(root->rightchild, newcoderight);
    }
    
    //the first code passed in was "" we can't call free on that
    if(strcmp(code, "") != 0){
       eecs345_free(code);
    }
    eecs345_free(root);
    return;
}

/* Read in a file.  Count the number of occurrences of each character in the file, and produce the Huffman tree and the Huffman codes for each character */
int main(int argc, char *argv[]) {
  FILE *infile;
  int i, c;
  int counts[256];

  if (argc != 2) {
     printf("Usage: %s inputfile\n", argv[0]);
     return 0;
  }

  /* Open a file.  For reading. */
  infile = fopen(argv[1], "r");

  /* Initialize the count of each character to 0 */
  for (i = 0; i < 256; i++)
    counts[i] = 0;

  /* Count the number of occurrences each character */
  while ((c = fgetc(infile)) != EOF)
    counts[c]++;
  
  /* create a heap for the huffman tree nodes */
  struct heap huffmanHeap = *initialize_heap(256, (void *)huffmanNodeCompare);

  /* add any a huffman tree node for each character in the file */
  for(i = 0; i < 256; i++){
    if(counts[i] > 0){
        add_to_heap(&huffmanHeap, initialize_node(i, counts[i]));
    }    
  }
  
  /* create the huffman tree from the heap */
  while(huffmanHeap.endptr > 1){
    //remove the first two nodes from the heap
    struct hufftreenode * left = heap_remove_min(&huffmanHeap);
    struct hufftreenode * right = heap_remove_min(&huffmanHeap);
    
    //create a new node that has the weight of the sum of the first two and has the first two as children
    struct hufftreenode * newParent = initialize_node(-1, left->weight + right->weight);
    newParent->leftchild = left;
    newParent->rightchild = right;
        
    //add the new node to the heap to generate the tree
    add_to_heap(&huffmanHeap, newParent);
  }
  
  //create the codes from the tree
  traverse((struct hufftreenode *)heap_get_min(&huffmanHeap), "");

  //print the codes for each character
  for (i = 0; i < 256; i++) {
    if (counts[i] != 0) {
      printf("%3c  %5d  %s\n", i, counts[i], huffmancodes[i]);
      eecs345_free(huffmancodes[i]);
    }
  }

  test_for_memoryleaks();
}


