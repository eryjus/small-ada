#include "small-ada-tree.h"

#include <cstring>

inline static string Indent(int i)
{
    char buf[256] = {0};

    while (i --) {
        strcat(buf, " ");
    }

    return string(buf);
}

DeclarationList *AdaPgm::GetDeclsAndDestroy(void)
{
    DeclarationList *rv = list;
    delete this;
    return rv;
}


void DeclarationList::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "DeclarationList = (\n";
    if (decl) decl->Print(os, i+1); else os << StartLine() << Indent(i+1) << "decl = (NULL)\n";
    if (next) next->Print(os, i+1); else os << StartLine() << Indent(i+1) << "next = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void WithDeclaration::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "WithDeclaraion = (\n";
    if (list) list->Print(os, i+1); else os << StartLine() << Indent(i+1) << "list = (NULL)\n";
    if (specs) specs->Print(os, i+1); else os << StartLine() << Indent(i+1) << "specs = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void AdaPgm::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "AdaPgm = (\n";
    os << StartLine() << Indent(i+1) << "fileName = \"" << fileName << "\"\n";
    if (list) list->Print(os, i+1); else os << StartLine() << Indent(i+1) << "list = (NULL)\n";
    if (next) next->Print(os, i+1); else os << StartLine() << Indent(i+1) << "next = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void PrivateDeclaration::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "PrivateDeclaration = (\n";
    os << StartLine() << Indent(i) << ")\n";
}

void QualNameList::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "QualNameList = (\n";
    if (names) names->Print(os, i+1); else os << StartLine() << Indent(i+1) << "names = (NULL)\n";
    if (next) next->Print(os, i+1); else os << StartLine() << Indent(i+1) << "next = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void UseDeclaration::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "UseDeclaration = (\n";
    if (uses) uses->Print(os, i+1); else os << StartLine() << Indent(i+1) << "uses = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void UseTypeDeclaration::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "UseTypeDeclaration = (\n";
    if (uses) uses->Print(os, i+1); else os << StartLine() << Indent(i+1) << "uses = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void ProcedureSpec::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "ProcedureSpec = (\n";
    if (name) name->Print(os, i+1); else os << StartLine() << Indent(i+1) << "name = (NULL)\n";
    if (parms) parms->Print(os, i+1); else os << StartLine() << Indent(i+1) << "parms = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void SubprogBody::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "SubprogBody = (\n";
    if (spec) spec->Print(os, i+1); else os << StartLine() << Indent(i+1) << "spec = (NULL)\n";
    if (decls) decls->Print(os, i+1); else os << StartLine() << Indent(i+1) << "decls = (NULL)\n";
    if (body) body->Print(os, i+1); else os << StartLine() << Indent(i+1) << "body = (NULL)\n";
    if (endID) endID->Print(os, i+1); else os << StartLine() << Indent(i+1) << "endIDs = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void BlockBody::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "BlockBody = (\n";
    if (stmts) stmts->Print(os, i+1); else os << StartLine() << Indent(i+1) << "stmts = (NULL)\n";
    if (excepts) excepts->Print(os, i+1); else os << StartLine() << Indent(i+1) << "excepts = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void StatementList::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "StatementList = (\n";
    if (stmt) stmt->Print(os, i+1); else os << StartLine() << Indent(i+1) << "stmt = (NULL)\n";
    if (next) next->Print(os, i+1); else os << StartLine() << Indent(i+1) << "next = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void ProcedureCall::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "ProcedureCall = (\n";
    if (call) call->Print(os, i+1); else os << StartLine() << Indent(i+1) << "call = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void ExpressionList::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "ExpressionList = (\n";
    if (expr) expr->Print(os, i+1); else os << StartLine() << Indent(i+1) << "expr = (NULL)\n";
    if (next) next->Print(os, i+1); else os << StartLine() << Indent(i+1) << "next = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void ArrayOrCall::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "ArrayOrCall = (\n";
    if (name) name->Print(os, i+1); else os << StartLine() << Indent(i+1) << "name = (NULL)\n";
    if (indexOrArgList) indexOrArgList->Print(os, i+1); else os << StartLine() << Indent(i+1) << "indexOrArgList = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void Identifier::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "Identifier = (" << id->GetKeyValue() << ")\n";
}

void StrLiteral::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "StrLiteral = (\"" << id->GetKeyValue() << "\")\n";
}

void IdentExpr::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "IdentExpr = (\n";
    if (id) id->Print(os, i+1); else os << StartLine() << Indent(i+1) << "id = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void StrExpr::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "StrExpr = (\n";
    if (str) str->Print(os, i+1); else os << StartLine() << Indent(i+1) << "str = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

void Package::Print(ostream &os, int i)
{
    os << StartLine() << Indent(i) << "Package = (\n";
    if (qName) qName->Print(os, i+1); else os << StartLine() << Indent(i+1) << "qName = (NULL)\n";
    if (decl) decl->Print(os, i+1); else os << StartLine() << Indent(i+1) << "decl = (NULL)\n";
    if (pvtDecl) pvtDecl->Print(os, i+1); else os << StartLine() << Indent(i+1) << "pvtDecl = (NULL)\n";
    if (endID) endID->Print(os, i+1); else os << StartLine() << Indent(i+1) << "endID = (NULL)\n";
    os << StartLine() << Indent(i) << ")\n";
}

