
// Compiler implementation of the D programming language
// Copyright (c) 1999-2012 by Digital Mars
// All Rights Reserved
// written by Walter Bright
// http://www.digitalmars.com
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.

// Handle macro implementation

#include <stdio.h>
#include <assert.h>

#include "dmacro.h"
#include "mars.h"
#include "init.h"
#include "declaration.h"
#include "attrib.h"
#include "expression.h"
#include "scope.h"
#include "mtype.h"
#include "aggregate.h"
#include "identifier.h"
#include "id.h"
#include "module.h"
#include "statement.h"
#include "template.h"
#include "hdrgen.h"

#ifdef IN_GCC
#include "d-dmd-gcc.h"
#endif

MacroDeclaration::MacroDeclaration (Loc start, Loc end, Identifier* id, StorageClass storageClass, Type* type)
    : FuncDeclaration(start, end, id, storageClass, type)
{
    //printf("MacroDeclaration(loc = %s) %s\n", loc.toChars(), toChars());
}

Dsymbol* MacroDeclaration::syntaxCopy (Dsymbol* symbol)
{
    MacroDeclaration* declaration;

    if (symbol)
        declaration = (MacroDeclaration*) symbol;

    else
        declaration = new MacroDeclaration(loc, endloc, ident, storage_class, type->syntaxCopy());

    declaration->outId = outId;
    declaration->frequire = frequire ? frequire->syntaxCopy() : NULL;
    declaration->fensure = fensure ? fensure->syntaxCopy() : NULL;
    declaration->fbody = fbody ? fbody->syntaxCopy() : NULL;

    assert(!fthrows); // deprecated

    return declaration;
}

void MacroDeclaration::semantic (Scope* sc)
{
    TypeFunction* typeFunction = (TypeFunction*) type;
    assert(typeFunction && typeFunction->ty == Tfunction);

    if (scope)
    {
        sc = scope;
        scope = NULL;
    }

    sc = sc->push();
    parent = sc->parent;
    // Dsymbol* parent = toParent2();
    // AggregateDeclaration* aggregateDeclaration = parent->isAggregateDeclaration();
    // 
    // Type* tret = aggregateDeclaration->handle;
    // assert(tret);
    // 
    // tret = tret->addStorageClass(storage_class | sc->stc)->addMod(type->mod);
    // typeFunction->next = tret;
    // type = type->semantic(loc, sc);
    // 
    // if (!originalType)
    //     originalType = type;

    FuncDeclaration::semantic(sc);
    sc->pop();
}

const char* MacroDeclaration::kind ()
{
    return "macro";
}

char* MacroDeclaration::toChars ()
{
    return (char*) "macro";
}

int MacroDeclaration::isVirtual ()
{
    return FALSE;
}