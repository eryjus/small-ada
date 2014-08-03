#include "StringTables.h"

#include <cstdlib>
#include <utility>
using namespace std;

IdentifierTable idTable;
NumberTable numTable;
StringTable strTable;

long int Entry::_NextID = 1;


string Entry::strlwr(const char *s)
{
    string rv;

    while (*s) {
        rv += tolower(*s);
        s ++;
    }

    return rv;
}
