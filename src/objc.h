
// Support functions for Objective-C integration with DMD
// Copyright (c) 2010 Michel Fortin
// All Rights Reserved
// http://michelf.com/
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.

#ifndef OBJC_H
#define OBJC_H

#include "root.h"
#include "mtype.h"
#include "stringtable.h"

struct Identifier;
struct Symbol;
struct FuncDeclaration;
struct ClassDeclaration;
struct InterfaceDeclaration;

struct ObjcSymbols
{
    static void init();

    static int hassymbols;

    static Symbol *msgSend;
    static Symbol *msgSend_stret;
    static Symbol *msgSend_fpret;
    static Symbol *msgSendSuper;
    static Symbol *msgSendSuper_stret;
    static Symbol *stringLiteralClassRef;
    static Symbol *siminfo;
    static Symbol *smodinfo;
    static Symbol *ssymmap;

    static StringTable *sclassnametable;
    static StringTable *sclassreftable;
    static StringTable *smethvarnametable;
    static StringTable *smethvarreftable;
    static StringTable *smethvartypetable;
    static StringTable *sprototable;

    static Symbol *getMsgSend(Type *ret, int hasHiddenArg);
    static Symbol *getMsgSendSuper(int hasHiddenArg);
    static Symbol *getStringLiteralClassRef();

    static Symbol *getCString(const char *str, size_t len, const char *symbolName);
    static Symbol *getUString(const void *str, size_t len, const char *symbolName);
    static Symbol *getImageInfo();
    static Symbol *getModuleInfo(ClassDeclarations *cls, ClassDeclarations *cat);
    static Symbol *getSymbolMap(ClassDeclarations *cls, ClassDeclarations *cat);

    static Symbol *getClassName(const char *str, size_t len);
    static Symbol *getClassName(Identifier *ident);
    static Symbol *getClassReference(const char *str, size_t len);
    static Symbol *getClassReference(Identifier *ident);

    static Symbol *getMethVarName(const char *str, size_t len);
    static Symbol *getMethVarName(Identifier *ident);
    static Symbol *getMethVarRef(const char *str, size_t len);
    static Symbol *getMethVarRef(Identifier *ident);
    static Symbol *getMethVarType(const char *str, size_t len);
    static Symbol *getMethVarType(Dsymbol **types, size_t dim);
    static Symbol *getMethVarType(Dsymbol *type);
    static Symbol *getMethVarType(FuncDeclaration *func);

    static Symbol *getProtocolSymbol(ClassDeclaration *interface);
    static Symbol *getStringLiteral(const void *str, size_t len, size_t sz);
};

// Helper class to efficiently build a selector from identifiers and colon tokens
struct ObjcSelectorBuilder
{
    size_t slen;
    Identifier *parts[10];
    size_t partCount;
    int colonCount;

    ObjcSelectorBuilder() { partCount = 0; colonCount = 0; slen = 0; }
    void addIdentifier(Identifier *id);
    void addColon();
    int isValid();
    const char *toString();
};

struct ObjcSelector
{
    static StringTable stringtable;
    static int incnum;

    const char *stringvalue;
    size_t stringlen;
    size_t paramCount;

    static void init ();

    ObjcSelector(const char *sv, size_t len, size_t pcount);
    Symbol *toNameSymbol();
    Symbol *toRefSymbol();
    elem *toElem();

    static ObjcSelector *lookup(ObjcSelectorBuilder *builder);
    static ObjcSelector *lookup(const char *s);
    static ObjcSelector *lookup(const char *s, size_t len, size_t pcount);
    static ObjcSelector *create(FuncDeclaration *fdecl);
};

struct ObjcClassRefExp : Expression
{
    ClassDeclaration *cdecl;

    ObjcClassRefExp(Loc loc, ClassDeclaration *cdecl);
    void toCBuffer(OutBuffer *buf, HdrGenState *hgs);
    elem *toElem(IRState *irs);
};

struct ObjcDotClassExp : UnaExp
{
    int noop; // !=0 if nothing needs to be done

    ObjcDotClassExp(Loc loc, Expression *e);
    Expression *semantic(Scope *sc);
    void toCBuffer(OutBuffer *buf, HdrGenState *hgs);
    elem *toElem(IRState *irs);

    static FuncDeclaration *classFunc();
};

struct ObjcProtocolOfExp : UnaExp
{
	InterfaceDeclaration *idecl;
	static ClassDeclaration *protocolClassDecl;

    ObjcProtocolOfExp(Loc loc, Expression *e);
    Expression *semantic(Scope *sc);
    void toCBuffer(OutBuffer *buf, HdrGenState *hgs);
    elem *toElem(IRState *irs);
};


struct ObjcClassDeclaration
{
    ClassDeclaration *cdecl;
    int ismeta;
    Symbol *symbol;
    Symbol *sprotocols;

    static ClassDeclaration *getObjcMetaClass(ClassDeclaration *cdecl);

    ObjcClassDeclaration(ClassDeclaration *cdecl, int ismeta = 0);
    void toObjFile(int multiobj);
    void toDt(dt_t **pdt);

    Symbol *getMetaclass();
    Symbol *getIVarList();
    Symbol *getMethodList();
    Symbol *getProtocolList();
};

struct ObjcProtocolDeclaration
{
    ClassDeclaration *idecl;
    Symbol *symbol;

    ObjcProtocolDeclaration(ClassDeclaration *idecl);
    void toObjFile(int multiobj);
    void toDt(dt_t **pdt);

    Symbol *getMethodList(int wantsClassMethods);
    Symbol *getProtocolList();
};

struct TypeObjcSelector : TypeNext
{
    // .next is a TypeFunction

    TypeObjcSelector(Type *t);
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    d_uns64 size(Loc loc);
    unsigned alignsize();
    MATCH implicitConvTo(Type *to);
    void toCBuffer2(OutBuffer *buf, HdrGenState *hgs, int mod);
    Expression *defaultInit(Loc loc);
    int isZeroInit(Loc loc);
    int checkBoolean();
    TypeInfoDeclaration *getTypeInfoDeclaration();
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    int hasPointers();
    TypeTuple *toArgTypes();
#if CPP_MANGLE
    void toCppMangle(OutBuffer *buf, CppMangleState *cms);
#endif

    type *toCtype();
};

#endif