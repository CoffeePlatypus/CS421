#include <stdlib.h>
#include <string.h>
#include "persons.h"
#include <stdio.h>

int main(int argc, char ** argv) {
  struct PersonList *listy = plCreate();
  plAdd(listy, personCreate("Rose","Froegel",18,"rfroegel5.99@gmail.com"));
  plAdd(listy, personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu"));

  plAdd(listy, personCreate("Joseph", "Froegel", 15, "Fang78913@gmail.com"));
  plAdd(listy, personCreate("Noah", "Froegel", 10, "NA"));
  plAdd(listy, personCreate("Julia", "2ndJulia", 20, "31415nard@gmail.com"));

  //plRemovePerson(listy, personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu"));
  printList(listy);
  //plDelete(listy, PD_DEEP);


//  printList(plFindByLastName(listy, "Froegel"));

  //printPerson(plGet(listy, 0));
//   printf("%d\n", plSize(listy));
//
// // test persons
//   struct Person * p1 = personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu");
//   struct Person * p2 = personCreate("Julia","Froegel",20,"froegel.julia@uwlax.edu");
// printf("%d\n", equalPeople(p1,p2) );
//   if(equalPeople(p1,p2)) {
//     printf("%s\n", "true" );
//   }
  //printf("First %s\n", personFirstName(p1));
  // printf("%s\n", personLastName(p1));
  //printf("%s\n", personEmail(p) );
  //printf("%d\n", personAge(p));
  //personDelete(p1);
  //printf("%s\n",toPrint); <-printf

}
