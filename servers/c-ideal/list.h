#pragma once

#include <pthread.h>

typedef struct list_node {
    void* value;
    struct list_node* next;
    void (*value_free)(void* value);
} list_node;

typedef struct list {
    list_node* first;
    list_node* last;
    pthread_mutex_t lock;
} list;



list* list_malloc(void);
void list_free(list* l);
void list_free_node_values(list* l);
void list_append(list* l, void* value, void (*value_free)(void*));
void list_append_with_reuse_list(list* l, void* value, void (*value_free)(void*), list* reuse_list);

#define list_for_each(l, pos) \
    for(pos = l->first; pos != NULL; pos = pos->next)
