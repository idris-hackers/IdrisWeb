#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "rand_c.h"


/* Massive cop-out so I don't have to mess around doing this in Idris! */
// Taken from a StackOverflow post by Abhay Budakoti
int random_number(int min_num, int max_num)
{
    int result=0,low_num=0,hi_num=0;
    if(min_num<max_num) {
        low_num=min_num;
        hi_num=max_num+1; // this is done to include max_num in output.
    } else {
        low_num=max_num+1;// this is done to include max_num in output.
        hi_num=min_num;
    }
    srand(time(NULL));
    result = (rand()%(hi_num-low_num))+low_num;
    return result;
}
