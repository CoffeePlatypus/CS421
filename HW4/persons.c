#include <stdlib.h>
#include <string.h>
#include "persons.h"
#include <stdio.h>

//Person ///////////////////////////////////////////////////////////////////

struct Person *personCreate(char *first, char *last, int age, char *email) {
  PerPoint person = (PerPoint) calloc(1,sizeof(PerPoint));
  person->first=strdup(first);
  person->last=strdup(last);
  person->age=age;
  person->email=strdup(email);
  return person;
}

void personDelete(struct Person *person) {
  free(person->first);
  free(person->last);
  free(person->email);
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
  ListPoint list = (ListPoint) calloc(1,sizeof(ListPoint));
  list->size=0;
  list->first=(NodePoint) calloc(1,sizeof(NodePoint));
  //list->first->value = (PerPoint) calloc(1,sizeof(PerPoint));
  return list;
}

void plDelete(struct PersonList *db, enum CopyType copyType) {
  NodePoint temp = db->first;
  NodePoint next = NULL;
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
  NodePoint temp = (NodePoint) calloc(1,sizeof(NodePoint));
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
    db->first = db->first->next;
    db->size--;
  }
  NodePoint temp = db->first;
  NodePoint next = temp->next;
  while(next->next) {
    if(equalPeople(next->value,p)) {
      db->size--;
      temp->next = next->next;
      next = temp->next;
    }else {
      temp = next;
      next = temp->next;
    }
  }
}

struct PersonList *plFindByFirstName(struct PersonList *db, char *name) {
  ListPoint result = plCreate();
  int i;
  NodePoint temp = db->first;
  for(i=0; i<db->size;i++) {
    if(!strcmp(personFirstName(temp->value), name)) {
      plAdd(result, temp->value);
    }
    temp = temp->next;
  }
  return result;
}

struct PersonList *plFindByLastName(struct PersonList *db, char *name) {
  ListPoint result = plCreate();
  int i;
  NodePoint temp = db->first;
  for(i=0; i<db->size;i++) {
    if(!strcmp(personLastName(temp->value), name)) {
      plAdd(result, temp->value);
    }
    temp = temp->next;
  }
  return result;
}

struct PersonList *plFindAtLeastAsOldAs(struct PersonList *db, int age) {
  ListPoint result = plCreate();
  int i;
  NodePoint temp = db->first;
  for(i=0; i<db->size;i++) {
    if(personAge(temp->value) >= age) {
      plAdd(result, temp->value);
    }
    temp = temp->next;
  }
  return result;
}

struct Person *plGet(struct PersonList *db, int n) {
  PerPoint p;
  int i;
  n++;
  NodePoint temp = db->first;
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
