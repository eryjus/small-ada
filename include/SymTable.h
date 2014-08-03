
#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "Lists.h"
#include "StringTables.h"

typedef enum {TP_UNKNOWN, TP_PACKAGE, TP_PROCEDURE} SymbolKind;

//
// -- This is a symbol entry in the symbol table
//    ------------------------------------------
class SymEntry {
private:
	SymbolKind kind;
	string mangledName;
	string package;
	Symbol name;
	SymEntry *ofType;

public:
	SymEntry(Symbol n) : name(n), ofType(NULL), package(""), mangledName(""), kind(TP_UNKNOWN) {}

public:
	void Print(void);
	Symbol GetName(void) const { return name; }

	void SetPackage(string &s) { package = s; }
	string GetPackage(void) const { return package; }
	string GetFullName(void) const { return package + "." + name->GetKeyValue(); }
	SymEntry *SetSymbolKind(SymbolKind k) { kind = k; return this; }
	SymbolKind GetSymbolKind(void) const { return kind; }
};

//
// -- This class is a scope for symbol entries
//    ----------------------------------------
class Scope {
	friend class SymbolTable;

protected:
	List<SymEntry> *entries;

protected:
	Scope() : entries(NULL) {}

protected:
	SymEntry *Lookup(Symbol name);
	bool ProbeScope(Symbol name);
	SymEntry *Insert(Symbol name);
	void DeleteScope();
	void Print(void);
};

//
// -- This class is a table of scopes (and therefore symbols)
//    -------------------------------------------------------
class SymbolTable {
protected:
	List<Scope> *scopes;

public:
	SymbolTable() : scopes(NULL) { EnterScope(); }

public:
	void EnterScope(void) { scopes = new List<Scope>(new Scope, scopes); }
	void LeaveScope(void);

	SymEntry *Lookup(Symbol name);
	SymEntry *Insert(Symbol name) { scopes->elem()->Insert(name); }
	bool ProbeScope(Symbol name);
	void Print(void);
};

//
// -- This class represents the entire typecheck environment
//    ------------------------------------------------------
class Environment {
private:
	SymbolTable *symbols;
	SymbolTable *types;
	List<Entry> *useList;

public:
	Environment() : symbols(new SymbolTable()), types(new SymbolTable()), useList(NULL) {}
	void EnterScope(void) { symbols->EnterScope(); types->EnterScope(); }
	void LeaveScope(void) { symbols->LeaveScope(); types->LeaveScope(); }

	SymEntry *Lookup(Symbol name);
	bool ProbeScope(Symbol name);
	void Print(void);

	SymEntry *AddPackageName(Symbol name);
	SymEntry *AddProcedure(Symbol name);
	void AddUse(Symbol name) { useList = new List<Entry>(name, useList); }
};

extern Environment *tcEnv;

#endif
