
#ifndef __LIST_H__
#define __LIST_H__

template <class T>
class List {
private:
    T *_elem;
    List<T> *_nextList;

public:
    List(T *h, List<T> *n) : _elem(h), _nextList(n) {};

public:
    T *elem(void) const { return _elem; };
    List<T> *next(void) const { return _nextList; };
};

#endif
