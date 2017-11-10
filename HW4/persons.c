#include <stdlib.h>
#include <string.h>
#include "persons.h"
#include <stdio.h>

//Person ///////////////////////////////////////////////////////////////////

struct Person *personCreate(char *first, char *last, int age, char *email) {
  struct Person *person = (struct Person *) malloc(sizeof(struct Person));
  person->first=first;
  person->last=last;
  person->age=age;
  person->email=email;
  return person;
}

void personDelete(struct Person *person) {
  printf("%s\n", "fail");
  free(person->first);  //doesnt work? why?
  printf("%s\n", "first");
  free(person->last);
  printf("%s\n", "last");
  free(&person->age); //where age is located?
  printf("%s\n", "age");
  free(person->email);
  printf("%s\n", "email");
  free(person);
}

char *personFirstName(struct Person *person) {
  return person->first;
}

char *personLastName(struct Person *person) {
  return person->last;
}

char *personEmail(struct Person *person) {
  return person->email;
}

int personAge(struct Person *person) {
  return person->age;
}
///////////////////////////////////////////////////////////////////////////

//Person List/////////////////////////////////////////////////////////////

struct PersonList *plCreate() {
  struct PersonList *list = (struct PersonList *) malloc(sizeof(struct PersonList));
  list->size=0;
  list->first=NULL;
  return list;
}

void plDelete(struct PersonList *db, enum CopyType copyType) {
  //if(copytype) { // pd deep -> delete people
  //}else{ //pd shallow -> list only delete
  //}
  struct Node *temp = db->first;
  struct Node *next;
  int i;
  for(i=0; i<db->size; i++) {
    if(copyType) {
      personDelete(temp->value);
    }
    next=temp->next;
    free(temp);
    temp=next;
  }
  free(&(db->size));
  free(db);
}

void plAdd(struct PersonList *db, struct Person *p) {
  struct Node *temp = (struct Node *) malloc(sizeof(struct Node));
  temp->next = db->first;
  temp->value = p;
  db->first=temp;
  db->size++;
}

void plRemove(struct PersonList *db, struct Person *p) {
  
}
