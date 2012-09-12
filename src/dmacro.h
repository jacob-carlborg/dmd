
// Compiler implementation of the D programming language
// Copyright (c) 1999-2012 by Digital Mars
// All Rights Reserved
// written by Walter Bright
// http://www.digitalmars.com
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.

#ifndef DMD_DMACRO_H
#define DMD_DMACRO_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

// #include "root.h"
// #include "arraytypes.h"
#include "declaration.h"
#include "dsymbol.h"

struct MacroDeclaration : FuncDeclaration
{
    MacroDeclaration (Loc start, Loc end, Identifier* id, StorageClass storageClass, Type* type);
    Dsymbol* syntaxCopy (Dsymbol* symbol);
    void semantic (Scope* sc);

    const char* kind ();
    char* toChars ();

    int isVirtual ();
    MacroDeclaration* isMacroDeclaration () { return this; }
};

#endif /* DMD_DMACRO_H */