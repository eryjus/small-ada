#ifndef __AST_ADD_ON_H__
#define __AST_ADD_ON_H__

#include "small-ada-tree.h"

// -- Templates for lists
//    -------------------
template <class T>
inline T *append(T *l, T *r) {
	T *rv = l;
	if (!l) return r; if (!r) return l;
	while(l->next) l = (T *)l->next;
	l->next = r;
	return rv;
}

template <class T>
inline T *first(T *l) {
	return (T*)l;
}

template <class T>
inline bool more(T *l) {
	return (l != 0);
}

template <class T>
inline T *next(T *l) {
	if (more((T *)l)) return (T *)l->next;
	else return (T *)0;
}

template <class T>
inline int len(T *i) {
	int rv; T *l;
	for (rv = 0, l = first((T *)i); more((T *)l); l = next((T *)l)) rv ++;
	return rv;
}

#define EMPTY(T) ((T *)0)
#define EMPTY_SYM ((Symbol)0)

class _Common {
public:
	virtual void semant(void) {};
};

#endif
