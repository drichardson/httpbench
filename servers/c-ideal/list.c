#include "list.h"

#include "likely.h"
#include "log.h"
#include <pthread.h>
#include <stdlib.h>

list* list_malloc(void) {
    list* l = (list*)calloc(1, sizeof(list));
    int rc = pthread_mutex_init(&l->lock, NULL);
    if (rc != 0) {
        log_with_errno(rc, "pthread_mutex_init failed");
    }
    return l;
}

void list_free(list* l) {
    for(list_node* n = l->first; n != NULL;) {
        if (n->value_free) {
            n->value_free(n->value);
        }
        list_node* n_to_free = n;
        n_to_free = n->next;
        free(n_to_free);
    }

    free(l);
}

void list_free_node_values(list* l) {
    list_node* n;
    list_for_each(l, n) {
        if (n->value_free) {
            n->value_free(n->value);
        }
        n->value = NULL;
    }
}


void list_append(list* l, void* value, void (*value_free)(void*)) {
    list_node* n = (list_node*)malloc(sizeof(list_node));
    if (likely(l->last)) {
        l->last->next = n;
        l->last = n;
    } else {
        l->first = l->last = n;
    }
} 

static list_node* list_pop_front_node(list* l) {
    list_node* n = l->first;
    if (n) {
        n->next = NULL;
        l->first = l->first->next;
    }
    return n;
}


void list_append_with_reuse_list(list* l, void* value, void (*value_free)(void*), list* reuse_list)
{
    list_node* n = list_pop_front_node(reuse_list);

    if (n == NULL) n = (list_node*)malloc(sizeof(list_node));

    if (likely(l->last)) {
        l->last->next = n;
        l->last = n;
    } else {
        l->first = l->last = n;
    }
}

