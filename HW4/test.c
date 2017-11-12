#include <stdlib.h>
#include <string.h>
#include "persons.h"
#include <stdio.h>

int main(int argc, char ** argv) {
  struct PersonList *listy = plCreate();
  plAdd(listy, personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu"));
  plAdd(listy, personCreate("Rose","Froegel",18,"rfroegel5.99@gmail.com"));
  plAdd(listy, personCreate("Joseph", "Froegel", 15, "Fang78913@gmail.com"));
  plAdd(listy, personCreate("Noah", "Froegel", 10, "NA"));
  plAdd(listy, personCreate("Julia", "2ndJulia", 20, "31415nard@gmail.com"));
  //printList(listy);

  //printList(plFindByLastName(listy, "Froegel"));

  printPerson(plGet(listy, 0));
  printf("%d\n", plSize(listy));

// test persons
  // struct Person * p1 = personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu");
  // printPerson(p1);
  // struct Person * p2 = personCreate("Julia", "Clare", 20, "dklsjdlfkjsd");
  // char * name ="Julia";
  // if(personFirstName(p1) == name) {
  //   printf("%s\n", "true" );
  // }
  //printf("First %s\n", personFirstName(p1));
  // printf("%s\n", personLastName(p1));
  //printf("%s\n", personEmail(p) );
  //printf("%d\n", personAge(p));
  //personDelete(p);
  //printf("%s\n",toPrint); <-printf

}
