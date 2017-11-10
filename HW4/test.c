#include <stdlib.h>
#include <string.h>
#include "persons.h"
#include <stdio.h>

int main(int argc, char ** argv) {

  struct Person * p = personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu");
  printf("%s\n", personFirstName(p));
  printf("%s\n", personLastName(p));
  printf("%s\n", personEmail(p) );
  printf("%d\n", personAge(p));
  personDelete(p);
  //printf("%s\n",toPrint); <-printf

}
