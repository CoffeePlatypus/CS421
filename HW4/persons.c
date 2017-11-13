#include <stdlib.h>
#include <string.h>
#include "persons.h"
#include <stdio.h>

//Person ///////////////////////////////////////////////////////////////////

struct Person *personCreate(char *first, char *last, int age, char *email) {
  struct Person *person = (struct Person *) malloc(sizeof(struct Person));
  person->first=strdup(first);
  person->last=strdup(last);
  person->age=age;
  person->email=strdup(email);
  return person;
}

void personDelete(struct Person *person) {
  free(person->first);  //now can only free first
  // printf("%s\n", "first");
  free(person->last);
  // printf("%s\n", "last");
  // free(&person->age);
  // printf("%s\n", "age");
  free(person->email);
  // printf("%s\n", "email");
  free(person); // <-this works
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
  list->first=(struct Node *) calloc(1,sizeof(struct Node));
  list->first->value = (struct Person *) calloc(1,sizeof(struct Person *));
  return list;
}

void plDelete(struct PersonList *db, enum CopyType copyType) {
  //if(copytype) { // pd deep -> delete people
  //}else{ //pd shallow -> list only delete
  //}
  struct Node *temp = db->first;
  struct Node *next = NULL;
  int i;
  for(i=0; i<db->size; i++) {
    //printf("%d\n", i);
    if(copyType) {
      personDelete(temp->value);
    }
    if(!next) {
      free(next);
    }
    next=temp->next;
    free(temp);
    temp=next;
  }
//printf("Loop\n");
  free(db);
}

void plAdd(struct PersonList *db, struct Person *p) {
  struct Node *temp = (struct Node *) malloc(sizeof(struct Node));
  temp->next = db->first;
  temp->value = p;
  db->first=temp;
  db->size++;
}

int equalPeople(struct Person *p1, struct Person *p2) {
  return ( !strcmp(personFirstName(p1), personFirstName(p2))
  && !strcmp(personLastName(p1), personLastName(p2))
  && !strcmp(personEmail(p1), personEmail(p2))
  && (personAge(p1) == personAge(p2)));
}

void plRemovePerson(struct PersonList*db, struct Person *p) {
  while(db->first && equalPeople(db->first->value, p)) {
    printf("%s\n", "remove" );
    db->first = db->first->next;
    db->size--;
  }
  struct Node *temp = db->first;
  struct Node *next = temp->next;
  while(next) {
    printPerson(next->value);
    if(equalPeople(next->value,p)) {
      printf("%s\n", "remove1");
      db->size--;
      temp->next = next->next;
      next = temp->next;
      printf("here\n" );
    }
    temp = next;
    next = temp->next;
    printf("heeoef\n" );
    if(next->value) {
      printf("%s\n", "no seg fault?" );
    }
  }


  // int i=0;
  // struct Node *temp = db->first;
  // while(equalPeople(temp->value, p)) {
  //   i++;
  //   temp = temp->next;
  // }
  // db->first = temp;
  // struct Node *next = temp->next;
  // while(i<=db->size) {
  //   i++;
  //   if (equalPeople(next->value, p)) {
  //     temp->next = next->next;
  //     next = temp->next;
  //   }
  // }
}

struct PersonList *plFindByFirstName(struct PersonList *db, char *name) {
  struct PersonList *result = plCreate();
  int i;
  struct Node *temp = db->first;
  for(i=0; i<db->size;i++) {
    if(!strcmp(personFirstName(temp->value), name)) {
      plAdd(result, temp->value);
    }
    temp = temp->next;
  }
  return result;
}

struct PersonList *plFindByLastName(struct PersonList *db, char *name) {
  struct PersonList *result = plCreate();
  int i;
  struct Node *temp = db->first;
  for(i=0; i<db->size;i++) {
    if(!strcmp(personLastName(temp->value), name)) {
      plAdd(result, temp->value);
    }
    temp = temp->next;
  }
  return result;
}

struct PersonList *plFindAtLeastAsOldAs(struct PersonList *db, int age) {
  struct PersonList *result = plCreate();
  int i;
  struct Node *temp = db->first;
  for(i=0; i<db->size;i++) {
    if(personAge(temp->value) >= age) {
      plAdd(result, temp->value);
    }
    temp = temp->next;
  }
  return result;
}

struct Person *plGet(struct PersonList *db, int n) {
  struct Person *p;
  int i;
  n++;
  struct Node *temp = db->first;
  for(i=0; i<=n; i++) {
    p=temp->value;
    temp->next;
  }
  return p;
}

int plSize(struct PersonList *db) {
  return db->size;
}

//Print Functions
void printPerson(struct Person * p) {
  printf("First Name: %s\n", personFirstName(p));
  printf("Last Name: %s\n", personLastName(p));
  printf("Age: %d\n", personAge(p));
  printf("Email: %s\n", personEmail(p));
}

void printList(struct PersonList *db) {
  int i;
  struct Node *temp = db->first;
  printf("%s\n", "" );
  for(i=0; i<(db->size); i++) {
    printPerson(temp->value);
    temp=temp->next;
    printf("%s\n", "" );
  }
}
