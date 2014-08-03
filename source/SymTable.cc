#include "SymTable.h"

Environment *tcEnv = new Environment();

SymEntry *Environment::Lookup(Symbol name)
{
	SymEntry *wrk;

	if ((wrk = symbols->Lookup(name)) != NULL) return wrk;
	if ((wrk = types->Lookup(name)) != NULL) return wrk;
	return NULL;
}

bool Environment::ProbeScope(Symbol name)
{
	bool w1, w2;

	w1 = symbols->ProbeScope(name);
	w2 = types->ProbeScope(name);

	if ((w1 || w2) && !(w1 && w2)) return true;
	else return false;
}

void Environment::Print(void)
{
	cout << "Symbols:" << endl;
	symbols->Print();
	cout << endl;
	cout << "Types:" << endl;
	types->Print();
}

void SymbolTable::LeaveScope(void)
{
	if (!scopes) return;

	scopes->elem()->DeleteScope();
	List<Scope> *wrk = scopes;
	scopes = scopes->next();

	delete wrk;
}

SymEntry *SymbolTable::Lookup(Symbol name)
{
	SymEntry *rv;
	List<Scope>* wrk = scopes;

	while (wrk && wrk->elem()) {
		rv = wrk->elem()->Lookup(name);
		if (rv) return rv;
		wrk = wrk->next();
	}

	return NULL;
}

bool SymbolTable::ProbeScope(Symbol name)
{
	return scopes->elem()->ProbeScope(name);
}

void SymbolTable::Print(void)
{
	List<Scope> *wrk = scopes;

	while (wrk) {
		wrk->elem()->Print();
		wrk = wrk->next();
	}
}

SymEntry *Scope::Lookup(Symbol name)
{
	List<SymEntry>* wrk = entries;

	while (wrk) {
		if (wrk->elem()->GetName() == name) return wrk->elem();
		wrk = wrk->next();
	}

	return NULL;
}

bool Scope::ProbeScope(Symbol name)
{
	return (Lookup(name) != NULL);
}

SymEntry *Scope::Insert(Symbol name)
{
	SymEntry *rv = new SymEntry(name);
	entries = new List<SymEntry>(rv, entries);

	return rv;
}

void Scope::DeleteScope()
{
	List<SymEntry>* wrk = entries;

	while (wrk) {
		entries = wrk->next();
		delete wrk;
		wrk = entries;
	}
}

void Scope::Print(void)
{
	List<SymEntry>* wrk = entries;

	while (wrk) {
		cout << wrk->elem()->GetName()->GetKeyValue() << endl;
		wrk = wrk->next();
	}
}

SymEntry *Environment::AddPackageName(Symbol name)
{
	SymEntry *rv = symbols->Lookup(name);

	if (rv) {
		if (rv->GetSymbolKind() != TP_PACKAGE) rv = NULL;
	} else {
		rv = symbols->Insert(name)->SetSymbolKind(TP_PACKAGE);
	}

	return rv;
}

SymEntry *Environment::AddProcedure(Symbol name)
{
	SymEntry *rv = symbols->Lookup(name);

	if (rv) {
		if (rv->GetSymbolKind() != TP_PROCEDURE) rv = NULL;
	} else {
		rv = symbols->Insert(name)->SetSymbolKind(TP_PROCEDURE);
	}

	return rv;
}
