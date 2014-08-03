
#ifndef __STRINGTABLES_H__
#define __STRINGTABLES_H__

#include <string>
#include <iostream>
using namespace std;

#include "Lists.h"

//
// -- Entry is a parent class to the table entries
//    --------------------------------------------
class Entry {
private:
    static long int _NextID;
    long int ID;
    string KeyValue;

public:
    string GetKeyValue(void) const { return KeyValue; };

public:
    Entry(const string &s) : KeyValue(s), ID(_NextID ++) {};

public:
    static string strlwr(const string &s) { return strlwr(s.c_str()); };
    static string strlwr(const char *s);
    bool Equals(const string &s) { return (s == KeyValue); };
    string GetString(void) const { return KeyValue; }
};

//---------------------------------------------------------------------------------------------

template <class T>
class _StringTable  {
private:
    List<T> *table;
    unsigned long _nextIndex;

public:
    _StringTable() : table(NULL), _nextIndex(1) {};

protected:
    T *add(const string &s) {
        T *rv = lookup(s);
        if (!rv) { rv = new T(s); table = new List<T>(rv, table); }
        return rv;
    };

public:
    T *lookup(const string &s) {
        List<T> *wrk = table;
        while (wrk && !wrk->elem()->Equals(s)) wrk = wrk->next();
        return (wrk?wrk->elem():NULL);
    };
};

//---------------------------------------------------------------------------------------------

//
// -- This is an identifier entry into the table
//    ------------------------------------------
class IdentEntry : public Entry {
public:
    IdentEntry(const string &id) : Entry(id) {};
    IdentEntry(const char *id) : Entry(id) {};
};

//---------------------------------------------------------------------------------------------

//
// -- This is the full identifier table
//    ---------------------------------
class IdentifierTable : public _StringTable<IdentEntry> {
public:
    IdentEntry *AddString(const char *s) { return add(Entry::strlwr(s)); }
    IdentEntry *AddString(string s) { return add(Entry::strlwr(s)); }
};

//---------------------------------------------------------------------------------------------

//
// -- This is an number entry into the table
//    --------------------------------------
class NumberEntry : public Entry {
public:
    NumberEntry(const string &num) : Entry(num) {};
    NumberEntry(const char *num) : Entry(num) {};
};

//---------------------------------------------------------------------------------------------

//
// -- This is the full number table
//    -----------------------------
class NumberTable : public _StringTable<NumberEntry> {
public:
    NumberEntry *AddString(const char *s) { return add(Entry::strlwr(s)); };
};

//---------------------------------------------------------------------------------------------

//
// -- This is a string entry into the table
//    -------------------------------------
class StringEntry : public Entry {
public:
    StringEntry(const string &s) : Entry(s) {};
    StringEntry(const char *s) : Entry(s) {};
};

//---------------------------------------------------------------------------------------------

//
// -- This is the full string table
//    -----------------------------
class StringTable : public _StringTable<StringEntry> {
public:
    StringEntry *AddString(const char *s) { return add(s); };
};

//
// -- A convenient way to refer to the individual table entries
//    ---------------------------------------------------------
typedef Entry *Symbol;

//
// -- Declare the tables
//    ------------------
extern IdentifierTable idTable;
extern NumberTable numTable;
extern StringTable strTable;

#endif
