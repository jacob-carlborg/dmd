
#include "objc.h"
#include "identifier.h"
#include "dsymbol.h"
#include "declaration.h"
#include "aggregate.h"
#include "target.h"
#include "id.h"
#include "attrib.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "objc_glue.h"

// Backend
#include "cc.h"
#include "dt.h"
#include "type.h"
#include "mtype.h"
#include "oper.h"
#include "global.h"
#include "mach.h"
#include "scope.h"

void mangleToBuffer(Type *t, OutBuffer *buf);

#define DMD_OBJC_ALIGN 2

static char* buildIVarName (ClassDeclaration* cdecl, VarDeclaration* ivar, size_t* resultLength)
{
    const char* className = cdecl->objc.ident->string;
    size_t classLength = cdecl->objc.ident->len;
    const char* ivarName = ivar->ident->string;
    size_t ivarLength = ivar->ident->len;

    // Ensure we have a long-enough buffer for the symbol name. Previous buffer is reused.
    static const char* prefix = "_OBJC_IVAR_$_";
    static size_t prefixLength = 13;
    static char* name;
    static size_t length;
    size_t requiredLength = prefixLength + classLength + 1 + ivarLength;

    if (requiredLength + 1 >= length)
    {
        length = requiredLength + 12;
        name = (char*) realloc(name, length);
    }

    // Create symbol name _OBJC_IVAR_$_<ClassName>.<IvarName>
    memmove(name, prefix, prefixLength);
    memmove(name + prefixLength, className, classLength);
    memmove(name + prefixLength + classLength + 1, ivarName, ivarLength);
    name[prefixLength + classLength] = '.';
    name[requiredLength] = 0;

    *resultLength = requiredLength;
    return name;
}

static const char* getTypeEncoding(Type* type)
{
    if (type == Type::tvoid)            return "v";
    else if (type == Type::tint8)       return "c";
    else if (type == Type::tuns8)       return "C";
    else if (type == Type::tchar)       return "C";
    else if (type == Type::tint16)      return "s";
    else if (type == Type::tuns16)      return "S";
    else if (type == Type::twchar)      return "S";
    else if (type == Type::tint32)      return "l";
    else if (type == Type::tuns32)      return "L";
    else if (type == Type::tdchar)      return "L";
    else if (type == Type::tint64)      return "q";
    else if (type == Type::tuns64)      return "Q";
    else if (type == Type::tfloat32)     return "f";
    else if (type == Type::timaginary32) return "f";
    else if (type == Type::tfloat64)     return "d";
    else if (type == Type::timaginary64) return "d";
    else if (type == Type::tfloat80)     return "d"; // "float80" is "long double" in Objective-C, but "long double" has no specific
    else if (type == Type::timaginary80) return "d"; // encoding character documented. Since @encode in Objective-C outputs "d", which is the same as "double", that's what we do here. But it doesn't look right.

    else                                 return "?"; // unknown
    // TODO: add "B" BOOL, "*" char*, "#" Class, "@" id, ":" SEL
    // TODO: add "^"<type> indirection and "^^" double indirection
}

int ObjcSymbols::hassymbols = 0;

Symbol *ObjcSymbols::msgSend = NULL;
Symbol *ObjcSymbols::msgSend_stret = NULL;
Symbol *ObjcSymbols::msgSend_fpret = NULL;
Symbol *ObjcSymbols::msgSendSuper = NULL;
Symbol *ObjcSymbols::msgSendSuper_stret = NULL;
Symbol *ObjcSymbols::msgSend_fixup = NULL;
Symbol *ObjcSymbols::msgSend_stret_fixup = NULL;
Symbol *ObjcSymbols::msgSend_fpret_fixup = NULL;
Symbol *ObjcSymbols::stringLiteralClassRef = NULL;
Symbol *ObjcSymbols::siminfo = NULL;
Symbol *ObjcSymbols::smodinfo = NULL;
Symbol *ObjcSymbols::ssymmap = NULL;
ObjcSymbols *ObjcSymbols::instance = NULL;

StringTable *ObjcSymbols::sclassnametable = NULL;
StringTable *ObjcSymbols::sclassreftable = NULL;
StringTable *ObjcSymbols::smethvarnametable = NULL;
StringTable *ObjcSymbols::smethvarreftable = NULL;
StringTable *ObjcSymbols::smethvartypetable = NULL;
StringTable *ObjcSymbols::sprototable = NULL;
StringTable *ObjcSymbols::sivarOffsetTable = NULL;
StringTable *ObjcSymbols::spropertyNameTable = NULL;
StringTable *ObjcSymbols::spropertyTypeStringTable = NULL;

static StringTable *initStringTable(StringTable *stringtable)
{
    delete stringtable;
    stringtable = new StringTable();
    stringtable->_init();

    return stringtable;
}

extern int seg_list[SEG_MAX];

void ObjcSymbols::init()
{
    if (global.params.isObjcNonFragileAbi)
        instance = new NonFragileAbiObjcSymbols();
    else
        instance = new FragileAbiObjcSymbols();

    hassymbols = 0;

    msgSend = NULL;
    msgSend_stret = NULL;
    msgSend_fpret = NULL;
    msgSendSuper = NULL;
    msgSendSuper_stret = NULL;
    stringLiteralClassRef = NULL;
    siminfo = NULL;
    smodinfo = NULL;
    ssymmap = NULL;

    // clear tables
    sclassnametable = initStringTable(sclassnametable);
    sclassreftable = initStringTable(sclassreftable);
    smethvarnametable = initStringTable(smethvarnametable);
    smethvarreftable = initStringTable(smethvarreftable);
    smethvartypetable = initStringTable(smethvartypetable);
    sprototable = initStringTable(sprototable);
    sivarOffsetTable = initStringTable(sivarOffsetTable);
    spropertyNameTable = initStringTable(spropertyNameTable);
    spropertyTypeStringTable = initStringTable(spropertyTypeStringTable);

    // also wipe out segment numbers
    for (int s = 0; s < SEG_MAX; ++s)
        seg_list[s] = 0;
}

Symbol *ObjcSymbols::getGlobal(const char* name)
{
    return symbol_name(name, SCglobal, type_fake(TYnptr));
}

Symbol *ObjcSymbols::getGlobal(const char* name, type* t)
{
    return symbol_name(name, SCglobal, t);
}

Symbol *ObjcSymbols::getFunction(const char* name)
{
    return getGlobal(name, type_fake(TYhfunc));
}

Symbol *ObjcSymbols::getMsgSend(Type *ret, int hasHiddenArg)
{
    if (hasHiddenArg)
    {   if (!msgSend_stret)
            msgSend_stret = symbol_name("_objc_msgSend_stret", SCglobal, type_fake(TYhfunc));
        return msgSend_stret;
    }
    else if (ret->isfloating())
    {   if (!msgSend_fpret)
            msgSend_fpret = symbol_name("_objc_msgSend_fpret", SCglobal, type_fake(TYnfunc));
        return msgSend_fpret;
    }
    else
    {   if (!msgSend)
            msgSend = symbol_name("_objc_msgSend", SCglobal, type_fake(TYnfunc));
        return msgSend;
    }
    assert(0);
    return NULL;
}

Symbol *ObjcSymbols::getMsgSendSuper(int hasHiddenArg)
{
    if (hasHiddenArg)
    {   if (!msgSendSuper_stret)
            msgSendSuper_stret = symbol_name("_objc_msgSendSuper_stret", SCglobal, type_fake(TYhfunc));
        return msgSendSuper_stret;
    }
    else
    {   if (!msgSendSuper)
            msgSendSuper = symbol_name("_objc_msgSendSuper", SCglobal, type_fake(TYnfunc));
        return msgSendSuper;
    }
    assert(0);
    return NULL;
}

Symbol *ObjcSymbols::getMsgSendFixup(Type* returnType, bool hasHiddenArg)
{
    if (hasHiddenArg)
    {
        if (!msgSend_stret_fixup)
            msgSend_stret_fixup = getFunction("_objc_msgSend_stret_fixup");
        return msgSend_stret_fixup;
    }
    else if (returnType->isfloating())
    {
        if (!msgSend_fpret_fixup)
            msgSend_fpret_fixup = getFunction("_objc_msgSend_fpret_fixup");
        return msgSend_fpret_fixup;
    }
    else
    {
        if (!msgSend_fixup)
            msgSend_fixup = getFunction("_objc_msgSend_fixup");
        return msgSend_fixup;
    }
    assert(0);
    return NULL;
}

Symbol *ObjcSymbols::getStringLiteralClassRef()
{
    if (!stringLiteralClassRef)
        stringLiteralClassRef = symbol_name("___CFConstantStringClassReference", SCglobal, type_fake(TYnptr));
    return stringLiteralClassRef;
}

Symbol *ObjcSymbols::getCString(const char *str, size_t len, const char *symbolName, ObjcSegment segment)
{
    hassymbols = 1;

    // create data
    dt_t *dt = NULL;
    dtnbytes(&dt, len + 1, str);

    // find segment
    int seg = objc_getsegment(segment);

    // create symbol
    Symbol *s;
    s = symbol_name(symbolName, SCstatic, type_allocn(TYarray, tschar));
    s->Sdt = dt;
    s->Sseg = seg;
    return s;
}

Symbol *ObjcSymbols::getUString(const void *str, size_t len, const char *symbolName)
{
    hassymbols = 1;

    // create data
    dt_t *dt = NULL;
    dtnbytes(&dt, (len + 1)*2, (const char *)str);

    // find segment
    int seg = objc_getsegment(SEGustring);

    // create symbol
    Symbol *s;
    s = symbol_name(symbolName, SCstatic, type_allocn(TYarray, tschar));
    s->Sdt = dt;
    s->Sseg = seg;
    return s;
}

Symbol *ObjcSymbols::getImageInfo()
{
    assert(!siminfo); // only allow once per object file
    hassymbols = 1;

    dt_t *dt = NULL;
    dtdword(&dt, 0); // version
    dtdword(&dt, global.params.isObjcNonFragileAbi ? 0 : 16); // flags

    siminfo = symbol_name("L_OBJC_IMAGE_INFO", SCstatic, type_allocn(TYarray, tschar));
    siminfo->Sdt = dt;
    siminfo->Sseg = objc_getsegment(SEGimage_info);
    outdata(siminfo);

    return siminfo;
}

Symbol *ObjcSymbols::getModuleInfo(ClassDeclarations *cls, ClassDeclarations *cat)
{
    assert(!smodinfo); // only allow once per object file
    smodinfo = instance->_getModuleInfo(cls, cat);
    ObjcSymbols::getImageInfo(); // make sure we also generate image info

    return smodinfo;
}

Symbol *ObjcSymbols::getSymbolMap(ClassDeclarations *cls, ClassDeclarations *cat)
{
    assert(!ssymmap); // only allow once per object file

    size_t classcount = cls->dim;
    size_t catcount = cat->dim;

    dt_t *dt = NULL;
    dtdword(&dt, 0); // selector refs count (unused)
    dtdword(&dt, 0); // selector refs ptr (unused)
    dtdword(&dt, classcount + (catcount << 16)); // class count / category count (expects little-endian)

    for (size_t i = 0; i < cls->dim; ++i)
        dtxoff(&dt, cls->tdata()[i]->objc.classSymbol, 0, TYnptr); // reference to class

    for (size_t i = 0; i < catcount; ++i)
        dtxoff(&dt, cat->tdata()[i]->objc.classSymbol, 0, TYnptr); // reference to category

    ssymmap = symbol_name("L_OBJC_SYMBOLS", SCstatic, type_allocn(TYarray, tschar));
    ssymmap->Sdt = dt;
    ssymmap->Sseg = objc_getsegment(SEGsymbols);
    outdata(ssymmap);

    return ssymmap;
}

Symbol *ObjcSymbols::getClassName(ObjcClassDeclaration* objcClass)
{
    return instance->_getClassName(objcClass);
}

Symbol *ObjcSymbols::getClassName(ClassDeclaration* cdecl, bool meta)
{
    ObjcClassDeclaration* objcClass = ObjcClassDeclaration::create(cdecl, meta);
    return ObjcSymbols::getClassName(objcClass);
}

Symbol *ObjcSymbols::getClassReference(ClassDeclaration* cdecl)
{
    hassymbols = 1;
    const char* s = cdecl->objc.ident->string;
    size_t len = cdecl->objc.ident->len;

    StringValue *sv = sclassreftable->update(s, len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        // create data
        dt_t *dt = NULL;
        Symbol *sclsname = getClassName(cdecl);
        dtxoff(&dt, sclsname, 0, TYnptr);

        // find segment for class references
        int seg = objc_getsegment(SEGcls_refs);

        static size_t classrefcount = 0;
        const char* prefix = global.params.isObjcNonFragileAbi ? "L_OBJC_CLASSLIST_REFERENCES_$_" : "L_OBJC_CLASS_REFERENCES_%lu";

        char namestr[42];
        sprintf(namestr, prefix, classrefcount++);
        sy = symbol_name(namestr, SCstatic, type_fake(TYnptr));
        sy->Sdt = dt;
        sy->Sseg = seg;
        outdata(sy);

        sv->ptrvalue = sy;
    }
    return sy;
}

Symbol *ObjcSymbols::getMethVarName(const char *s, size_t len)
{
    hassymbols = 1;

    StringValue *sv = smethvarnametable->update(s, len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        static size_t classnamecount = 0;
        char namestr[42];
        sprintf(namestr, "L_OBJC_METH_VAR_NAME_%lu", classnamecount++);
        sy = getCString(s, len, namestr, SEGmethname);
        sv->ptrvalue = sy;
    }
    return sy;
}

Symbol *ObjcSymbols::getMethVarName(Identifier *ident)
{
    return getMethVarName(ident->string, ident->len);
}

Symbol *ObjcSymbols::getMethVarRef(const char *s, size_t len)
{
    hassymbols = 1;

    StringValue *sv = smethvarreftable->update(s, len);
    Symbol *refsymbol = (Symbol *) sv->ptrvalue;
    if (refsymbol == NULL)
    {
        // create data
        dt_t *dt = NULL;
        Symbol *sselname = getMethVarName(s, len);
        dtxoff(&dt, sselname, 0*0x9877660, TYnptr);

        // find segment
        int seg = objc_getsegment(SEGselrefs);

        // create symbol
        static size_t selcount = 0;
        char namestr[42];
        sprintf(namestr, "L_OBJC_SELECTOR_REFERENCES_%lu", selcount);
        refsymbol = symbol_name(namestr, SCstatic, type_fake(TYnptr));

        refsymbol->Sdt = dt;
        refsymbol->Sseg = seg;
        outdata(refsymbol);
        sv->ptrvalue = refsymbol;

        ++selcount;
    }
    return refsymbol;
}

Symbol *ObjcSymbols::getMethVarRef(Identifier *ident)
{
    return getMethVarRef(ident->string, ident->len);
}


Symbol *ObjcSymbols::getMethVarType(const char *s, size_t len)
{
    hassymbols = 1;

    StringValue *sv = smethvartypetable->update(s, len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        static size_t classnamecount = 0;
        char namestr[42];
        sprintf(namestr, "L_OBJC_METH_VAR_TYPE_%lu", classnamecount++);
        sy = getCString(s, len, namestr, SEGmethtype);
        sv->ptrvalue = sy;
        outdata(sy);
    }
    return sy;
}

Symbol *ObjcSymbols::getMethVarType(Dsymbol **types, size_t dim)
{
    // Ensure we have a long-enough buffer for the symbol name. Previous buffer is reused.
    static char *typecode = NULL;
    static size_t typecode_cap = 0;
    size_t typecode_len = 0;

    for (size_t i = 0; i < dim; ++i) {
        Type *type;

        if (FuncDeclaration* func = types[i]->isFuncDeclaration())
            type = func->type->nextOf();
        else
            type = types[i]->getType();

        const char *typestr = getTypeEncoding(type);

        // Append character
        // Ensure enough length
        if (typecode_len + 1 >= typecode_cap)
        {   typecode_cap += typecode_len + 12;
            typecode = (char *)realloc(typecode, typecode_cap);
        }
        typecode[typecode_len] = typestr[0];
        ++typecode_len;
    }

    if (typecode_len + 1 >= typecode_cap)
    {   typecode_cap += typecode_len + 12;
        typecode = (char *)realloc(typecode, typecode_cap);
    }
    typecode[typecode_len] = 0; // zero-terminated

    return getMethVarType(typecode, typecode_len);
}

Symbol *ObjcSymbols::getMethVarType(FuncDeclaration *func)
{
    static Dsymbol **types;
    static size_t types_dim;

    size_t param_dim = func->parameters ? func->parameters->dim : 0;
    if (types_dim < 1 + param_dim)
    {   types_dim = 1 + param_dim + 8;
        types = (Dsymbol **)realloc(types, types_dim * sizeof(Dsymbol **));
    }
    types[0] = func; // return type first
    if (param_dim)
        memcpy(types+1, func->parameters->tdata(), param_dim * sizeof(Dsymbol **));

    return getMethVarType(types, 1 + param_dim);
}

Symbol *ObjcSymbols::getMethVarType(Dsymbol *s)
{
    return getMethVarType(&s, 1);
}

Symbol *ObjcSymbols::getMessageReference(ObjcSelector* selector, Type* returnType, bool hasHiddenArg)
{
    assert(selector->usesVTableDispatch());
    hassymbols = 1;

    Symbol* msgSendFixup = ObjcSymbols::getMsgSendFixup(returnType, hasHiddenArg);
    Symbol* selectorSymbol = getMethVarName(selector->stringvalue, selector->stringlen);
    size_t msgSendFixupLength = strlen(msgSendFixup->Sident);
    size_t fixupSelectorLength = 0;
    const char* fixupSelector = ObjcSelectorBuilder::fixupSelector(selector, msgSendFixup->Sident, msgSendFixupLength, &fixupSelectorLength);

    StringValue *sv = smethvarreftable->update(fixupSelector, fixupSelectorLength);
    Symbol *refsymbol = (Symbol *) sv->ptrvalue;
    if (refsymbol == NULL)
    {
        // create data
        dt_t* dt = NULL;
        dtxoff(&dt, msgSendFixup, 0, TYnptr);
        dtxoff(&dt, selectorSymbol, 0, TYnptr);

        // find segment
        int segment = objc_getsegment(SEGmessage_refs);

        // create symbol
        refsymbol = symbol_name(fixupSelector, SCstatic, type_fake(TYnptr));
        refsymbol->Sdt = dt;
        refsymbol->Sseg = segment;
        refsymbol->Salignment = 16;
        outdata(refsymbol);
        sv->ptrvalue = refsymbol;
    }
    return refsymbol;
}

Symbol *ObjcSymbols::getProtocolSymbol(ClassDeclaration *interface)
{
    hassymbols = 1;

    assert(interface->objc.meta == 0);

    StringValue *sv = sprototable->update(interface->objc.ident->string, interface->objc.ident->len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        ObjcProtocolDeclaration* p = ObjcProtocolDeclaration::create(interface);
        p->toObjFile(0);
        sy = p->symbol;
        sv->ptrvalue = sy;
    }
    return sy;
}


Symbol *ObjcSymbols::getStringLiteral(const void *str, size_t len, size_t sz)
{
    hassymbols = 1;

    // Objective-C NSString literal (also good for CFString)
    static size_t strcount = 0;
    char namestr[24];
    sprintf(namestr, "l_.str%lu", strcount);
    Symbol *sstr;
    if (sz == 1)
        sstr = getCString((const char *)str, len, namestr);
    else
        sstr = getUString(str, len, namestr);

    dt_t *dt = NULL;
    dtxoff(&dt, getStringLiteralClassRef(), 0, TYnptr);
    dtdword(&dt, sz == 1 ? 1992 : 2000);

    if (global.params.isObjcNonFragileAbi)
        dtdword(&dt, 0); // .space 4

    dtxoff(&dt, sstr, 0, TYnptr);
    dtsize_t(&dt, len);

    sprintf(namestr, "L__unnamed_cfstring_%lu", strcount++);
    Symbol *si = symbol_name(namestr, SCstatic, type_fake(TYnptr));
    si->Sdt = dt;
    si->Sseg = objc_getsegment(SEGcfstring);
    outdata(si);
    return si;
}

Symbol *ObjcSymbols::getPropertyName(const char* str, size_t len)
{
    hassymbols = 1;
    StringValue* sv = spropertyNameTable->update(str, len);
    Symbol* symbol = (Symbol*) sv->ptrvalue;

    if (!symbol)
    {
        static size_t propertyNameCount = 0;
        char nameStr[42];
        sprintf(nameStr, "L_OBJC_PROP_NAME_ATTR_%lu", propertyNameCount++);
        symbol = getCString(str, len, nameStr);
        sv->ptrvalue = symbol;
    }

    return symbol;
}

Symbol *ObjcSymbols::getPropertyName(Identifier* ident)
{
    return getPropertyName(ident->string, ident->len);
}

Symbol *ObjcSymbols::getPropertyTypeString(FuncDeclaration* property)
{
    assert(property->objc.isProperty());

    TypeFunction* type = (TypeFunction*) property->type;
    Type* propertyType = type->next->ty != TYvoid ? type->next : (*type->parameters)[0]->type;
    const char* typeEncoding = getTypeEncoding(propertyType);
    size_t len = strlen(typeEncoding);
    size_t nameLength = 1 + len;

    // Method encodings are not handled
    char* name = (char*) malloc(nameLength + 1);
    name[0] = 'T';
    memmove(name + 1, typeEncoding, len);
    name[nameLength] = 0;

    return getPropertyName(name, nameLength);
}

// MARK: FragileAbiObjcSymbols

Symbol *FragileAbiObjcSymbols::_getModuleInfo(ClassDeclarations *cls, ClassDeclarations *cat)
{
    dt_t *dt = NULL;

    dtdword(&dt, 7);  // version
    dtdword(&dt, 16); // size
    dtxoff(&dt, ObjcSymbols::getCString("", 0, "L_CLASS_NAME_"), 0, TYnptr); // name
    dtxoff(&dt, ObjcSymbols::getSymbolMap(cls, cat), 0, TYnptr); // symtabs

    Symbol* symbol = symbol_name("L_OBJC_MODULE_INFO", SCstatic, type_allocn(TYarray, tschar));
    symbol->Sdt = dt;
    symbol->Sseg = objc_getsegment(SEGmodule_info);
    outdata(symbol);

    return symbol;
}

Symbol* FragileAbiObjcSymbols::_getClassName(ObjcClassDeclaration *objcClass)
{
    hassymbols = 1;
    ClassDeclaration* cdecl = objcClass->cdecl;
    const char* s = cdecl->objc.ident->string;
    size_t len = cdecl->objc.ident->len;

    StringValue *sv = sclassnametable->update(s, len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        static size_t classnamecount = 0;
        char namestr[42];
        sprintf(namestr, "L_OBJC_CLASS_NAME_%lu", classnamecount++);
        sy = getCString(s, len, namestr, SEGclassname);
        sv->ptrvalue = sy;
    }
    return sy;
}

// MARK: NonFragileAbiObjcSymbols

NonFragileAbiObjcSymbols *NonFragileAbiObjcSymbols::instance = NULL;

NonFragileAbiObjcSymbols::NonFragileAbiObjcSymbols()
{
    emptyCache = NULL;
    emptyVTable = NULL;
    instance = (NonFragileAbiObjcSymbols*) ObjcSymbols::instance;
}

Symbol *NonFragileAbiObjcSymbols::getClassNameRo(Identifier* ident)
{
    return getClassNameRo(ident->string, ident->len);
}

Symbol *NonFragileAbiObjcSymbols::getClassNameRo(const char *s, size_t len)
{
    hassymbols = 1;

    StringValue *sv = sclassnametable->update(s, len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        static size_t classnamecount = 0;
        char namestr[42];
        sprintf(namestr, "L_OBJC_CLASS_NAME_%lu", classnamecount++);
        sy = getCString(s, len, namestr, SEGclassname);
        sv->ptrvalue = sy;
    }
    return sy;
}

Symbol *NonFragileAbiObjcSymbols::getIVarOffset(ClassDeclaration* cdecl, VarDeclaration* ivar, bool outputSymbol)
{
    hassymbols = 1;

    size_t length;
    const char* name = buildIVarName(cdecl, ivar, &length);
    StringValue* stringValue = sivarOffsetTable->update(name, length);
    Symbol* symbol = (Symbol*) stringValue->ptrvalue;

    if (!symbol)
    {
        symbol = getGlobal(name);
        stringValue->ptrvalue = symbol;
        symbol->Sfl |= FLextern;
    }

    if (outputSymbol)
    {
        dt_t* dt = NULL;
        dtsize_t(&dt, ivar->offset);

        symbol->Sdt = dt;
        symbol->Sseg = objc_getsegment(SEGobjc_ivar);
        symbol->Sfl &= ~FLextern;

        outdata(symbol);
    }

    return  symbol;
}

Symbol *NonFragileAbiObjcSymbols::getEmptyCache()
{
    hassymbols = 1;

    return emptyCache = emptyCache ? emptyCache : getGlobal("__objc_empty_cache");
}

Symbol *NonFragileAbiObjcSymbols::getEmptyVTable()
{
    hassymbols = 1;

    return emptyVTable = emptyVTable ? emptyVTable : getGlobal("__objc_empty_vtable");
}

Symbol *NonFragileAbiObjcSymbols::_getModuleInfo(ClassDeclarations *cls, ClassDeclarations *cat)
{
    dt_t *dt = NULL;

    for (size_t i = 0; i < cls->dim; i++)
        dtxoff(&dt, ObjcSymbols::getClassName((*cls)[i]), 0, TYnptr);

    for (size_t i = 0; i < cat->dim; i++)
        dtxoff(&dt, ObjcSymbols::getClassName((*cat)[i]), 0, TYnptr);

    Symbol* symbol = symbol_name("L_OBJC_LABEL_CLASS_$", SCstatic, type_allocn(TYarray, tschar));
    symbol->Sdt = dt;
    symbol->Sseg = objc_getsegment(SEGmodule_info);
    outdata(symbol);

    return symbol;
}

Symbol* NonFragileAbiObjcSymbols::_getClassName(ObjcClassDeclaration *objcClass)
{
    hassymbols = 1;
    ClassDeclaration* cdecl = objcClass->cdecl;
    const char* s = cdecl->objc.ident->string;
    size_t len = cdecl->objc.ident->len;

    const char* prefix = objcClass->ismeta ? "_OBJC_METACLASS_$_" : "_OBJC_CLASS_$_";
    const size_t prefixLength = objcClass->ismeta ? 18 : 14;
    s = prefixSymbolName(s, len, prefix, prefixLength);
    len += prefixLength;

    StringValue *sv = sclassnametable->update(s, len);
    Symbol *sy = (Symbol *) sv->ptrvalue;
    if (!sy)
    {
        sy = getGlobal(s);
        sv->ptrvalue = sy;
    }
    return sy;
}

// MARK: ObjcSelectorBuilder

const char* ObjcSelectorBuilder::fixupSelector (ObjcSelector* selector, const char* fixupName, size_t fixupLength, size_t* fixupSelectorLength)
{
    assert(selector->usesVTableDispatch());

    size_t length = 1 + fixupLength + 1 + selector->stringlen + 1; // + 1 for the 'l' prefix, '_' and trailing \0
    char* fixupSelector = (char*) malloc(length * sizeof(char));
    fixupSelector[0] = 'l';
    size_t position = 1;

    memcpy(fixupSelector + position, fixupName, fixupLength);
    position += fixupLength;
    fixupSelector[position] = '_';
    position++;

    memcpy(fixupSelector + position, selector->mangledStringValue, selector->stringlen);
    fixupSelector[length - 1] = '\0';

    *fixupSelectorLength = length - 1;
    return fixupSelector;
}

void ObjcSelectorBuilder::addIdentifier(Identifier *id)
{
    assert(partCount < 10);
    parts[partCount] = id;
    slen += id->len;
    partCount += 1;
}

void ObjcSelectorBuilder::addColon()
{
    slen += 1;
    colonCount += 1;
}

int ObjcSelectorBuilder::isValid()
{
    if (colonCount == 0)
        return partCount == 1;
    else
        return partCount >= 1 && partCount <= colonCount;
}

const char *ObjcSelectorBuilder::buildString(char separator)
{
    char *s = (char*)malloc(slen + 1);
    size_t spos = 0;
    for (size_t i = 0; i < partCount; ++i)
    {
        memcpy(&s[spos], parts[i]->string, parts[i]->len);
        spos += parts[i]->len;
        if (colonCount)
        {   s[spos] = separator;
            spos += 1;
        }
    }
    assert(colonCount == 0 || partCount <= colonCount);
    if (colonCount > partCount)
    {
        for (size_t i = 0; i < colonCount - partCount; ++i)
        {   s[spos] = separator;
            spos += 1;
        }
    }
    assert(slen == spos);
    s[slen] = '\0';
    return s;
}


// MARK: Selector

StringTable ObjcSelector::stringtable;
StringTable ObjcSelector::vTableDispatchSelectors;
int ObjcSelector::incnum = 0;

void ObjcSelector::init ()
{
    stringtable._init();
    vTableDispatchSelectors._init();

    if (global.params.isObjcNonFragileAbi)
    {
        vTableDispatchSelectors.insert("alloc", 5);
        vTableDispatchSelectors.insert("class", 5);
        vTableDispatchSelectors.insert("self", 4);
        vTableDispatchSelectors.insert("isFlipped", 9);
        vTableDispatchSelectors.insert("length", 6);
        vTableDispatchSelectors.insert("count", 5);

        vTableDispatchSelectors.insert("allocWithZone:", 14);
        vTableDispatchSelectors.insert("isKindOfClass:", 14);
        vTableDispatchSelectors.insert("respondsToSelector:", 19);
        vTableDispatchSelectors.insert("objectForKey:", 13);
        vTableDispatchSelectors.insert("objectAtIndex:", 14);
        vTableDispatchSelectors.insert("isEqualToString:", 16);
        vTableDispatchSelectors.insert("isEqual:", 8);

        // These three use vtable dispatch if the Objective-C GC is disabled
        vTableDispatchSelectors.insert("retain", 6);
        vTableDispatchSelectors.insert("release", 7);
        vTableDispatchSelectors.insert("autorelease", 11);

        // These three use vtable dispatch if the Objective-C GC is enabled
        // vTableDispatchSelectors.insert("hash", 4);
        // vTableDispatchSelectors.insert("addObject:", 10);
        // vTableDispatchSelectors.insert("countByEnumeratingWithState:objects:count:", 42);
    }
}

ObjcSelector::ObjcSelector(const char *sv, size_t len, size_t pcount, const char* mangled)
{
    stringvalue = sv;
    stringlen = len;
    paramCount = pcount;
    mangledStringValue = mangled;
}

ObjcSelector *ObjcSelector::lookup(ObjcSelectorBuilder *builder)
{
    const char* stringValue = builder->toString();
    const char* mangledStringValue = NULL;

    if (ObjcSelector::isVTableDispatchSelector(stringValue, builder->slen))
        mangledStringValue = builder->toMangledString();

    return lookup(stringValue, builder->slen, builder->colonCount, mangledStringValue);
}

ObjcSelector *ObjcSelector::lookup(const char *s)
{
	size_t len = 0;
	size_t pcount = 0;
	const char *i = s;
	while (*i != 0)
	{	++len;
		if (*i == ':') ++pcount;
		++i;
	}
	return lookup(s, len, pcount);
}

ObjcSelector *ObjcSelector::lookup(const char *s, size_t len, size_t pcount, const char* mangled)
{
    StringValue *sv = stringtable.update(s, len);
    ObjcSelector *sel = (ObjcSelector *) sv->ptrvalue;
    if (!sel)
    {
        sel = new ObjcSelector(sv->toDchars(), len, pcount, mangled);
        sv->ptrvalue = sel;
    }
    return sel;
}

ObjcSelector *ObjcSelector::create(FuncDeclaration *fdecl)
{
    OutBuffer buf;
    size_t pcount = 0;
    TypeFunction *ftype = (TypeFunction *)fdecl->type;

    // Special case: property setter
    if (ftype->isproperty && ftype->parameters && ftype->parameters->dim == 1)
    {   // rewrite "identifier" as "setIdentifier"
        char firstChar = fdecl->ident->string[0];
        if (firstChar >= 'a' && firstChar <= 'z')
            firstChar = firstChar - 'a' + 'A';

        buf.write("set", 3);
        buf.writeByte(firstChar);
        buf.write(fdecl->ident->string+1, fdecl->ident->len-1);
        buf.writeByte(':');
        goto Lcomplete;
    }

    // write identifier in selector
    buf.write(fdecl->ident->string, fdecl->ident->len);

    // add mangled type and colon for each parameter
    if (ftype->parameters && ftype->parameters->dim)
    {
        buf.writeByte('_');
        Parameters *arguments = ftype->parameters;
        size_t dim = Parameter::dim(arguments);
        for (size_t i = 0; i < dim; i++)
        {
            Parameter *arg = Parameter::getNth(arguments, i);
            mangleToBuffer(arg->type, &buf);
            buf.writeByte(':');
        }
        pcount = dim;
    }
Lcomplete:
    buf.writeByte('\0');

    return lookup((const char *)buf.data, buf.size, pcount);
}

bool ObjcSelector::isVTableDispatchSelector(const char* selector, size_t length)
{
    return global.params.isObjcNonFragileAbi && vTableDispatchSelectors.lookup(selector, length) != NULL;
}

Symbol *ObjcSelector::toNameSymbol()
{
    return ObjcSymbols::getMethVarName(stringvalue, stringlen);
}

Symbol *ObjcSelector::toRefSymbol()
{
    return ObjcSymbols::getMethVarRef(stringvalue, stringlen);
}

elem *ObjcSelector::toElem()
{
    return el_var(toRefSymbol());
}

// MARK: Class References

ObjcClassRefExp::ObjcClassRefExp(Loc loc, ClassDeclaration *cdecl)
    : Expression(loc, TOKobjcclsref, sizeof(ObjcClassRefExp))
{
    this->cdecl = cdecl;
    this->type = ObjcClassDeclaration::getObjcMetaClass(cdecl)->getType();
}

// MARK: .class Expression

ObjcDotClassExp::ObjcDotClassExp(Loc loc, Expression *e)
    : UnaExp(loc, TOKobjc_dotclass, sizeof(ObjcDotClassExp), e)
{
    noop = 0;
}

Expression *ObjcDotClassExp::semantic(Scope *sc)
{
    if (Expression *ex = unaSemantic(sc))
        return ex;

    if (e1->type && e1->type->ty == Tclass)
    {
        ClassDeclaration *cd = ((TypeClass *)e1->type)->sym;
        if (cd->objc.objc)
        {
            if (e1->op = TOKtype)
            {
                if (cd->isInterfaceDeclaration())
                {
                    error("%s is an interface type and has no static 'class' property", e1->type->toChars());
                    return new ErrorExp();
                }
                return new ObjcClassRefExp(loc, cd);
            }
            else if (cd->objc.meta)
            {   // this is already a class object, nothing to do
                noop = 1;
                type = cd->type;
                return this;
            }
            else
            {   // this is a regular (non-class) object, invoke class method
                type = cd->objc.metaclass->type;
                return this;
            }
        }
    }

    error("%s of type %s has no 'class' property", e1->toChars(), e1->type->toChars());
    return new ErrorExp();
}

// MARK: .interface Expression

ClassDeclaration *ObjcProtocolOfExp::protocolClassDecl = NULL;

ObjcProtocolOfExp::ObjcProtocolOfExp(Loc loc, Expression *e)
    : UnaExp(loc, TOKobjc_dotprotocolof, sizeof(ObjcProtocolOfExp), e)
{
    idecl = NULL;
}

Expression *ObjcProtocolOfExp::semantic(Scope *sc)
{
    if (Expression *ex = unaSemantic(sc))
        return ex;

    if (e1->type && e1->type->ty == Tclass)
    {
        ClassDeclaration *cd = ((TypeClass *)e1->type)->sym;
        if (cd->objc.objc)
        {
            if (e1->op = TOKtype)
            {
                if (cd->isInterfaceDeclaration())
                {
                    if (protocolClassDecl)
                    {
                        idecl = (InterfaceDeclaration *)cd;
                        type = protocolClassDecl->type;
                        return this;
                    }
                    else
                    {
                        error("'protocolof' property not available because its the 'Protocol' Objective-C class is not defined (did you forget to import objc.types?)");
                        return new ErrorExp();
                    }
                }
            }
        }
    }

    error("%s of type %s has no 'protocolof' property", e1->toChars(), e1->type->toChars());
    return new ErrorExp();
}

// MARK: ObjcClassDeclaration

ObjcClassDeclaration *ObjcClassDeclaration::create(ClassDeclaration *cdecl, int ismeta)
{
    if (global.params.isObjcNonFragileAbi)
        return new NonFragileAbiObjcClassDeclaration(cdecl, ismeta);
    else
        return new FragileAbiObjcClassDeclaration(cdecl, ismeta);
}

/* ClassDeclaration::metaclass contains the metaclass from the semantic point
 of view. This function returns the metaclass from the Objective-C runtime's
 point of view. Here, the metaclass of a metaclass is the root metaclass, not
 nil, and the root metaclass's metaclass is itself. */
ClassDeclaration *ObjcClassDeclaration::getObjcMetaClass(ClassDeclaration *cdecl)
{
    if (!cdecl->objc.metaclass && cdecl->objc.meta)
    {
        if (cdecl->baseClass)
            return getObjcMetaClass(cdecl->baseClass);
        else
            return cdecl;
    }
    else
        return cdecl->objc.metaclass;
}

ObjcClassDeclaration::ObjcClassDeclaration(ClassDeclaration *cdecl, int ismeta)
{
    this->cdecl = cdecl;
    this->ismeta = ismeta;
    symbol = NULL;
    sprotocols = NULL;
    sproperties = NULL;
}

Symbol *NonFragileAbiObjcClassDeclaration::getIVarOffset(VarDeclaration* ivar)
{
    if (ivar->toParent() == cdecl)
        return NonFragileAbiObjcSymbols::instance->getIVarOffset(cdecl, ivar, false);

    else if (cdecl->baseClass)
        return NonFragileAbiObjcClassDeclaration(cdecl->baseClass).getIVarOffset(ivar);

    else
        assert(false || "Trying to get the base class of root class");
}

// MARK: ObjcProtocolDeclaration

ObjcProtocolDeclaration* ObjcProtocolDeclaration::create(ClassDeclaration *idecl)
{
    if (global.params.isObjcNonFragileAbi)
        return new NonFragileAbiObjcProtocolDeclaration(idecl);
    else
        return new FragileAbiObjcProtocolDeclaration(idecl);
}

ObjcProtocolDeclaration::ObjcProtocolDeclaration(ClassDeclaration *idecl)
{
    this->idecl = idecl;
    symbol = NULL;
}

/***************************** TypeObjcSelector *****************************/

TypeObjcSelector::TypeObjcSelector(Type *t)
    : TypeNext(Tobjcselector, t)
{
    assert(((TypeFunction *)t)->linkage == LINKobjc);
}

Type *TypeObjcSelector::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    if (t == next)
        t = this;
    else
    {   t = new TypeObjcSelector(t);
        t->mod = mod;
    }
    return t;
}

Type *TypeObjcSelector::semantic(Loc loc, Scope *sc)
{
    if (deco)                   // if semantic() already run
    {
        //printf("already done\n");
        return this;
    }
    Scope* newScope = new Scope(*sc);
    newScope->linkage = LINKobjc;
    next = next->semantic(loc,newScope);

    return merge();
}

d_uns64 TypeObjcSelector::size(Loc loc)
{
    return Target::ptrsize;
}

unsigned TypeObjcSelector::alignsize()
{
    return Target::ptrsize;
}

MATCH TypeObjcSelector::implicitConvTo(Type *to)
{
    //printf("TypeDelegate::implicitConvTo(this=%p, to=%p)\n", this, to);
    //printf("from: %s\n", toChars());
    //printf("to  : %s\n", to->toChars());
    if (this == to)
        return MATCHexact;
#if 0 // not allowing covariant conversions because it interferes with overriding
    if (to->ty == Tdelegate && this->nextOf()->covariant(to->nextOf()) == 1)
        return MATCHconvert;
#endif
    return MATCHnomatch;
}

Expression *TypeObjcSelector::defaultInit(Loc loc)
{
#if LOGDEFAULTINIT
    printf("TypeObjcSelector::defaultInit() '%s'\n", toChars());
#endif
    return new NullExp(loc, this);
}

bool TypeObjcSelector::isZeroInit(Loc loc)
{
    return true;
}

bool TypeObjcSelector::checkBoolean()
{
    return true;
}

Expression *TypeObjcSelector::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
#if LOGDOTEXP
    printf("TypeDelegate::dotExp(e = '%s', ident = '%s')\n", e->toChars(), ident->toChars());
#endif
/*    if (ident == Id::ptr)
    {
        e->type = tvoidptr;
        return e;
    }
    else if (ident == Id::funcptr)
    {
        e = e->addressOf(sc);
        e->type = tvoidptr;
        e = new AddExp(e->loc, e, new IntegerExp(PTRSIZE));
        e->type = tvoidptr;
        e = new PtrExp(e->loc, e);
        e->type = next->pointerTo();
        return e;
    }
    else*/
    {
        e = Type::dotExp(sc, e, ident, flag);
    }
    return e;
}

int TypeObjcSelector::hasPointers()
{
    return FALSE; // not in GC memory
}

TypeInfoDeclaration *TypeObjcSelector::getTypeInfoDeclaration()
{
    return TypeInfoObjcSelectorDeclaration::create(this);
}

/***************************************/

#include "cond.h"
#include "expression.h"
#include "init.h"
#include "module.h"
#include "parse.h"
#include "statement.h"
#include "typinf.c"
#include "utf.h"

elem *addressElem(elem *e, Type *t, bool alwaysCopy = false);
unsigned totym(Type *tx);

Objc_StructDeclaration::Objc_StructDeclaration()
{
    selectorTarget = false;
    isSelector = false;
}

Objc_ClassDeclaration::Objc_ClassDeclaration(ClassDeclaration* cdecl, const char* msg)
{
    this->cdecl = cdecl;
    objc = false;
    meta = false;
    extern_ = false;
    hasPreinit = false;
    takesStringLiteral = false;
    ident = NULL;
    classSymbol = NULL;
    methods = NULL;
    metaclass = NULL;
}

bool Objc_ClassDeclaration::isInterface()
{
    return objc;
}

bool Objc_ClassDeclaration::isRootClass()
{
    return isInterface() && !metaclass && !cdecl->baseClass;
}

// MARK: Ojbc_FuncDeclaration

Ojbc_FuncDeclaration::Ojbc_FuncDeclaration(FuncDeclaration* fdecl)
{
    this->fdecl = fdecl;
    selector = NULL;
    vcmd = NULL;
}

void Ojbc_FuncDeclaration::createSelector()
{
    if (selector == NULL && fdecl->linkage == LINKobjc && fdecl->isVirtual() && fdecl->type)
    {
        TypeFunction *ftype = (TypeFunction *)fdecl->type;
        selector = ObjcSelector::create(fdecl);
    }
}

bool Ojbc_FuncDeclaration::isProperty()
{
    TypeFunction* t = (TypeFunction*)fdecl->type;

    return (fdecl->storage_class & STCproperty) &&
        t && t->parameters &&
        ((t->parameters->dim == 1 && t->next == Type::tvoid) ||
        (t->parameters->dim == 0 && t->next != Type::tvoid));
}

// MARK: TypeInfoObjcSelectorDeclaration

TypeInfoObjcSelectorDeclaration::TypeInfoObjcSelectorDeclaration(Type *tinfo)
: TypeInfoDeclaration(tinfo, 0)
{
    type = Type::typeinfodelegate->type;
}

TypeInfoObjcSelectorDeclaration *TypeInfoObjcSelectorDeclaration::create(Type *tinfo)
{
    return new TypeInfoObjcSelectorDeclaration(tinfo);
}

// MARK: ObjcSelectorExp

ObjcSelectorExp::ObjcSelectorExp(Loc loc, FuncDeclaration *f, int hasOverloads)
: Expression(loc, TOKobjcselector, sizeof(ObjcSelectorExp))
{
    this->func = f;
    this->selname = NULL;
    this->hasOverloads = hasOverloads;
}

ObjcSelectorExp::ObjcSelectorExp(Loc loc, char *selname, int hasOverloads)
: Expression(loc, TOKobjcselector, sizeof(ObjcSelectorExp))
{
    this->func = NULL;
    this->selname = selname;
    this->hasOverloads = hasOverloads;
}

Expression *ObjcSelectorExp::semantic(Scope *sc)
{
#if LOGSEMANTIC
    printf("ObjcSelectorExp::semantic('%s')\n", toChars());
#endif
    if (!type)
    {
        type = new TypeObjcSelector(func->type);
        type = type->semantic(loc, sc);
        if (!func->needThis())
        {   error("%s isn't a member function, has no selector", func->toChars());
            return new ErrorExp();
        }
        ClassDeclaration *cd = func->toParent()->isClassDeclaration();
        if (!cd->objc.objc)
        {   error("%s isn't an Objective-C class, function has no selector", cd->toChars());
            return new ErrorExp();
        }
    }
    return this;
}

// MARK: semantic

void objc_ClassDeclaration_semantic_PASSinit_LINKobjc(ClassDeclaration *self)
{
#if DMD_OBJC
    self->objc.objc = true;
    self->objc.extern_ = true;
#else
    self->error("Objective-C classes not supported");
#endif
}

void objc_ClassDeclaration_semantic_SIZEOKnone(ClassDeclaration *self, Scope *sc)
{
    if (self->objc.objc && !self->objc.meta && !self->objc.metaclass)
    {
        if (!self->objc.ident)
            self->objc.ident = self->ident;

        if (self->objc.ident == Id::Protocol)
        {
            if (ObjcProtocolOfExp::protocolClassDecl == NULL)
                ObjcProtocolOfExp::protocolClassDecl = self;
            else if (ObjcProtocolOfExp::protocolClassDecl != self)
                self->error("duplicate definition of Objective-C class '%s'", Id::Protocol);
        }

        // Create meta class derived from all our base's metaclass
        BaseClasses *metabases = new BaseClasses();
        for (size_t i = 0; i < self->baseclasses->dim; ++i)
        {
            ClassDeclaration *basecd = ((BaseClass *)self->baseclasses->data[i])->base;
            assert(basecd);
            if (basecd->objc.objc)
            {
                assert(basecd->objc.metaclass);
                assert(basecd->objc.metaclass->objc.meta);
                assert(basecd->objc.metaclass->type->ty == Tclass);
                assert(((TypeClass *)basecd->objc.metaclass->type)->sym == basecd->objc.metaclass);
                BaseClass *metabase = new BaseClass(basecd->objc.metaclass->type, PROTpublic);
                metabase->base = basecd->objc.metaclass;
                metabases->push(metabase);
            }
            else
                self->error("base class and interfaces for an Objective-C class must be extern (Objective-C)");
        }
        self->objc.metaclass = new ClassDeclaration(self->loc, Id::Class, metabases);
        self->objc.metaclass->storage_class |= STCstatic;
        self->objc.metaclass->objc.objc = true;
        self->objc.metaclass->objc.meta = true;
        self->objc.metaclass->objc.extern_ = self->objc.extern_;
        self->objc.metaclass->objc.ident = self->objc.ident;
        self->members->push(self->objc.metaclass);
        self->objc.metaclass->addMember(sc, self, 1);
    }
}

void objc_ClassDeclaration_semantic_staticInitializers(ClassDeclaration *self, Scope *sc2, size_t members_dim)
{
    if (self->objc.objc && !self->objc.extern_ && !self->objc.meta)
    {
        // Look for static initializers to create initializing function if needed
        Expression *inite = NULL;
        for (size_t i = 0; i < members_dim; i++)
        {
            VarDeclaration *vd = ((Dsymbol *)self->members->data[i])->isVarDeclaration();
            if (vd && vd->toParent() == self &&
                ((vd->init && !vd->init->isVoidInitializer()) && (vd->init || !vd->getType()->isZeroInit())))
            {
                Expression *thise = new ThisExp(vd->loc);
                thise->type = self->type;
                Expression *ie = vd->init->toExpression();
                if (!ie)
                    ie = vd->type->defaultInit(self->loc);
                if (!ie)
                    continue; // skip
                Expression *ve = new DotVarExp(vd->loc, thise, vd);
                ve->type = vd->type;
                Expression *e = new AssignExp(vd->loc, ve, ie);
                e->op = TOKblit;
                e->type = ve->type;
                inite = inite ? new CommaExp(self->loc, inite, e) : e;
            }
        }

        TypeFunction *tf = new TypeFunction(new Parameters, self->type, 0, LINKd);
        FuncDeclaration *initfd = self->findFunc(Id::_dobjc_preinit, tf);

        if (inite)
        {
            // we have static initializers, need to create any '_dobjc_preinit' instance
            // method to handle them.
            FuncDeclaration *newinitfd = new FuncDeclaration(self->loc, self->loc, Id::_dobjc_preinit, STCundefined, tf);
            Expression *retvale;
            if (initfd)
            {
                // call _dobjc_preinit in superclass
                retvale = new CallExp(self->loc, new DotIdExp(self->loc, new SuperExp(self->loc), Id::_dobjc_preinit));
                retvale->type = self->type;
            }
            else
            {
                // no _dobjc_preinit to call in superclass, just return this
                retvale = new ThisExp(self->loc);
                retvale->type = self->type;
            }
            newinitfd->fbody = new ReturnStatement(self->loc, new CommaExp(self->loc, inite, retvale));
            self->members->push(newinitfd);
            newinitfd->addMember(sc2, self, 1);
            newinitfd->semantic(sc2);

            // replace initfd for next step
            initfd = newinitfd;
        }

        if (initfd)
        {
            // replace alloc functions with stubs ending with a call to _dobjc_preinit
            // this is done by the backend glue in objc.c, we just need to set a flag
            self->objc.hasPreinit = true;
        }
    }
}

void objc_ClassDeclaration_semantic_invariant(ClassDeclaration *self, Scope *sc2)
{
    if (self->objc.objc && !self->objc.extern_ && !self->objc.meta)
    {
        // invariant for Objective-C class is handled by adding a _dobjc_invariant
        // dynamic method calling the invariant function and then the parent's
        // _dobjc_invariant if applicable.
        if (self->invs.dim > 0)
        {
            Loc iloc = self->inv->loc;
            TypeFunction *invtf = new TypeFunction(new Parameters, Type::tvoid, 0, LINKobjc);
            FuncDeclaration *invfd = self->findFunc(Id::_dobjc_invariant, invtf);

            // create dynamic dispatch handler for invariant
            FuncDeclaration *newinvfd = new FuncDeclaration(iloc, iloc, Id::_dobjc_invariant, STCundefined, invtf);
            if (self->baseClass && self->baseClass->inv)
                newinvfd->storage_class |= STCoverride;

            Expression *e;
            e = new DsymbolExp(iloc, self->inv);
            e = new CallExp(iloc, e);
            if (invfd)
            {   // call super's _dobjc_invariant
                e = new CommaExp(iloc, e, new CallExp(iloc, new DotIdExp(iloc, new SuperExp(iloc), Id::_dobjc_invariant)));
            }
            newinvfd->fbody = new ExpStatement(iloc, e);
            self->members->push(newinvfd);
            newinvfd->addMember(sc2, self, 1);
            newinvfd->semantic(sc2);
        }
    }
}

void objc_InterfaceDeclaration_semantic_objcExtern(InterfaceDeclaration *self, Scope *sc)
{
    if (sc->linkage == LINKobjc)
    {
#if DMD_OBJC
        self->objc.objc = true;
        // In the abscense of a better solution, classes with Objective-C linkage
        // are only a declaration. A class that derives from one with Objective-C
        // linkage but which does not have Objective-C linkage itself will
        // generate a definition in the object file.
        self->objc.extern_ = true; // this one is only a declaration

        if (!self->objc.ident)
            self->objc.ident = self->ident;
#else
        self->error("Objective-C interfaces not supported");
#endif
    }
}

ControlFlow objc_InterfaceDeclaration_semantic_mixingObjc(InterfaceDeclaration *self, Scope *sc, size_t i, TypeClass *tc)
{
    // Check for mixin Objective-C and non-Objective-C interfaces
    if (!self->objc.objc && tc->sym->objc.objc)
    {
        if (i == 0)
        {
            // This is the first -- there's no non-Objective-C interface before this one.
            // Implicitly switch this interface to Objective-C.
            self->objc.objc = true;
        }
        else
            goto Lobjcmix; // same error as below
    }
    else if (self->objc.objc && !tc->sym->objc.objc)
    {
    Lobjcmix:
        self->error("cannot mix Objective-C and non-Objective-C interfaces");
        self->baseclasses->remove(i);
        return CFcontinue;
    }

    return CFnone;
}

void objc_InterfaceDeclaration_semantic_createMetaclass(InterfaceDeclaration *self, Scope *sc)
{
    if (self->objc.objc && !self->objc.meta && !self->objc.metaclass)
    {   // Create meta class derived from all our base's metaclass
        BaseClasses *metabases = new BaseClasses();
        for (size_t i = 0; i < self->baseclasses->dim; ++i)
        {
            ClassDeclaration *basecd = ((BaseClass *)self->baseclasses->data[i])->base;
            assert(basecd);
            InterfaceDeclaration *baseid = basecd->isInterfaceDeclaration();
            assert(baseid);
            if (baseid->objc.objc)
            {
                assert(baseid->objc.metaclass);
                assert(baseid->objc.metaclass->objc.meta);
                assert(baseid->objc.metaclass->type->ty == Tclass);
                assert(((TypeClass *)baseid->objc.metaclass->type)->sym == baseid->objc.metaclass);
                BaseClass *metabase = new BaseClass(baseid->objc.metaclass->type, PROTpublic);
                metabase->base = baseid->objc.metaclass;
                metabases->push(metabase);
            }
            else
                self->error("base interfaces for an Objective-C interface must be extern (Objective-C)");
        }
        self->objc.metaclass = new InterfaceDeclaration(self->loc, Id::Class, metabases);
        self->objc.metaclass->storage_class |= STCstatic;
        self->objc.metaclass->objc.objc = true;
        self->objc.metaclass->objc.meta = true;
        self->objc.metaclass->objc.extern_ = self->objc.extern_;
        self->objc.metaclass->objc.ident = self->objc.ident;
        self->


        members->push(self->objc.metaclass);
        self->objc.metaclass->addMember(sc, self, 1);
    }
}

ControlFlow objc_StringExp_semantic(StringExp *self, Expression *&error)
{
    // determine if this string is pure ascii
    int ascii = 1;
    for (size_t i = 0; i < self->len; ++i)
    {
        if (((unsigned char *)self->string)[i] & 0x80)
        {
            ascii = 0;
            break;
        }
    }

    if (!ascii)
    {   // use UTF-16 for non-ASCII strings
        OutBuffer buffer;
        size_t newlen = 0;
        const char *p;
        size_t u;
        unsigned c;

        for (u = 0; u < self->len;)
        {
            p = utf_decodeChar((unsigned char *)self->string, self->len, &u, &c);
            if (p)
            {
                self->error("%s", p);
                error = new ErrorExp();
                return CFreturn;
            }
            else
            {
                buffer.writeUTF16(c);
                newlen++;
                if (c >= 0x10000)
                    newlen++;
            }
        }
        buffer.writeUTF16(0);
        self->string = buffer.extractData();
        self->len = newlen;
        self->sz = 2;
    }
    self->committed = 1;
    return CFnone;
}

ControlFlow objc_NewExp_semantic_alloc(NewExp *self, Scope *sc, ClassDeclaration *cd)
{
    if (cd->objc.meta)
    {
        self->error("cannot instanciate meta class '%s'", cd->toChars());
        return CFgoto;
    }

    // use Objective-C 'alloc' function
    Dsymbol *s = cd->search(self->loc, Id::alloc, 0);
    if (s)
    {
        FuncDeclaration *allocf = s->isFuncDeclaration();
        if (allocf)
        {
            allocf = resolveFuncCall(self->loc, sc, allocf, NULL, NULL, self->newargs);
            if (!allocf->isStatic())
            {
                self->error("function %s must be static to qualify as an allocator for Objective-C class %s", allocf->toChars(), cd->toChars());
                return CFgoto;
            }
            else if (((TypeFunction *)allocf->type)->next != allocf->parent->isClassDeclaration()->type)
            {
                self->error("function %s should return %s instead of %s to qualify as an allocator for Objective-C class %s",
                      allocf->toChars(), allocf->parent->isClassDeclaration()->type->toChars(),
                      ((TypeFunction *)allocf->type)->next->toChars(), cd->toChars());
                return CFgoto;
            }

            self->objcalloc = allocf;
        }
    }
    if (self->objcalloc == NULL)
    {
        self->error("no matching 'alloc' function in Objective-C class %s", cd->toChars());
        return CFgoto;
    }

    return CFnone;
}

ControlFlow objc_IsExp_semantic_TOKobjcselector(IsExp *self, Type *&tded)
{
    if (self->targ->ty != Tobjcselector)
        return CFgoto;
    tded = ((TypeObjcSelector *)self->targ)->next; // the underlying function type
    return CFbreak;
}

void objc_IsExp_semantic_TOKreturn_selector(IsExp *self, Type *&tded)
{
    tded = ((TypeDelegate *)self->targ)->next;
    tded = ((TypeFunction *)tded)->next;
}

void objc_CallExp_semantic_opOverload_selector(CallExp *self, Scope *sc, Type *t1)
{
    assert(self->argument0 == NULL);
    TypeObjcSelector *sel = (TypeObjcSelector *)t1;

    // harvest first argument and check if valid target for a selector
    int validtarget = 0;
    if (self->arguments->dim >= 1)
    {
        self->argument0 = ((Expression *)self->arguments->data[0])->semantic(sc);
        if (self->argument0 && self->argument0->type->ty == Tclass)
        {
            TypeClass *tc = (TypeClass *)self->argument0->type;
            if (tc && tc->sym && tc->sym->objc.objc)
                validtarget = 1; // Objective-C object
        }
        else if (self->argument0 && self->argument0->type->ty == Tpointer)
        {
            TypePointer *tp = (TypePointer *)self->argument0->type;
            if (tp->next->ty == Tstruct)
            {
                TypeStruct *ts = (TypeStruct *)tp->next;
                if (ts && ts->sym && ts->sym->objc.selectorTarget)
                    validtarget = 1; // struct with objc_selectortarget pragma applied
            }
        }
    }
    if (validtarget)
    {   // take first argument and use it as 'this'
        // create new array of expressions omiting first argument
        Expressions *newargs = new Expressions();
        for (int i = 1; i < self->arguments->dim; ++i)
            newargs->push(self->arguments->tdata()[i]);
        assert(newargs->dim == self->arguments->dim - 1);
        self->arguments = newargs;
    }
    else
        self->error("calling a selector needs an Objective-C object as the first argument");
}

void objc_CallExp_semantic_noFunction_selector(Type *t1, TypeFunction *&tf, const char *&p)
{
    TypeObjcSelector *td = (TypeObjcSelector *)t1;
    assert(td->next->ty == Tfunction);
    tf = (TypeFunction *)(td->next);
    p = "Objective-C selector";
}

ObjcSelectorExp * objc_AddrExp_semantic_TOKdotvar_selector(AddrExp *self, DotVarExp *dve, FuncDeclaration *f)
{
    return new ObjcSelectorExp(self->loc, f, dve->hasOverloads);
}

Expression * objc_AddrExp_semantic_TOKvar_selector(AddrExp *self, Scope *sc, VarExp *ve, FuncDeclaration *f)
{
    Expression *e = new ObjcSelectorExp(self->loc, f, ve->hasOverloads);
    e = e->semantic(sc);
    return e;
}

void objc_FuncDeclaration_semantic_checkAbstractStatic(FuncDeclaration *self)
{
    // Because static functions are virtual in Objective-C objects
    if (self->isAbstract() && self->isStatic() && self->linkage == LINKobjc)
        self->error("static functions cannot be abstract");
}

void objc_FuncDeclaration_semantic_parentForStaticMethod(FuncDeclaration *self, Dsymbol *&parent, ClassDeclaration *&cd)
{
    // Handle Objective-C static member functions, which are virtual
    // functions of the metaclass, by changing the parent class
    // declaration to the metaclass.
    if (cd->objc.objc && self->isStatic())
    {
        if (!cd->objc.meta) // but check that it hasn't already been done
        {
            assert(cd->objc.metaclass);
            parent = cd = cd->objc.metaclass;
        }
    }
}

void objc_FuncDeclaration_semantic_checkInheritedSelector(FuncDeclaration *self, ClassDeclaration *cd)
{
    if (cd->objc.objc)
    {
        // Check for Objective-C selector inherited form overriden functions
        for (size_t i = 0; i < self->foverrides.dim; ++i)
        {
            FuncDeclaration *foverride = (FuncDeclaration *)self->foverrides.data[i];
            if (foverride && foverride->objc.selector)
            {
                if (!self->objc.selector)
                    self->objc.selector = foverride->objc.selector; // inherit selector
                else if (self->objc.selector != foverride->objc.selector)
                    self->error("Objective-C selector %s must be the same as selector %s in overriden function.", self->objc.selector->stringvalue, foverride->objc.selector->stringvalue);
            }
        }
    }
}

void objc_FuncDeclaration_semantic_addClassMethodList(FuncDeclaration *self, ClassDeclaration *cd)
{
    if (cd->objc.objc)
    {
        // Add to class method lists
        self->objc.createSelector(); // create a selector if needed
        if (self->objc.selector && cd)
        {
            assert(self->isStatic() ? cd->objc.meta : !cd->objc.meta);

            cd->objc.methodList.push(self);
            if (cd->objc.methods == NULL)
            {
                cd->objc.methods = new StringTable;
                cd->objc.methods->_init();
            }
            StringValue *sv = cd->objc.methods->update(self->objc.selector->stringvalue, self->objc.selector->stringlen);

            if (sv->ptrvalue)
            {
                // check if the other function with the same selector is
                // overriden by this one
                FuncDeclaration *selowner = (FuncDeclaration *)sv->ptrvalue;
                if (selowner != self && !self->overrides(selowner))
                    self->error("Objcective-C selector '%s' already in use by function '%s'.", self->objc.selector->stringvalue, selowner->toChars());
            }
            else
                sv->ptrvalue = self;
        }
    }
}

void objc_FuncDeclaration_semantic_checkLinkage(FuncDeclaration *self)
{
    if (self->linkage != LINKobjc && self->objc.selector)
        self->error("function must have Objective-C linkage to attach a selector");
}

void objc_SynchronizedStatement_semantic_sync_enter(ClassDeclaration *cd, Parameters* args, FuncDeclaration *&fdenter)
{
    if (cd && cd->objc.objc) // replace with Objective-C's equivalent function
        fdenter = FuncDeclaration::genCfunc(args, Type::tvoid, Id::objc_sync_enter);
}

void objc_SynchronizedStatement_semantic_sync_exit(ClassDeclaration *cd, Parameters* args, FuncDeclaration *&fdexit)
{
    if (cd && cd->objc.objc) // replace with Objective-C's equivalent function
        fdexit = FuncDeclaration::genCfunc(args, Type::tvoid, Id::objc_sync_exit);
}

// MARK: FuncDeclaration

void objc_FuncDeclaration_declareThis(FuncDeclaration *self, Scope *sc, VarDeclaration** vobjccmd, VarDeclaration *v)
{
    if (vobjccmd && self->objc.selector)
    {
        VarDeclaration* varObjc = new VarDeclaration(self->loc, Type::tvoidptr, Id::_cmd, NULL);
        varObjc->storage_class |= STCparameter;
        varObjc->semantic(sc);
        if (!sc->insert(varObjc))
            assert(0);
        varObjc->parent = self;
        *vobjccmd = varObjc;

        assert(*vobjccmd != v);
    }
}

void objc_FuncDeclaration_isThis(FuncDeclaration *self, AggregateDeclaration *&ad)
{
    // Use Objective-C class object as 'this'
    ClassDeclaration *cd = self->isMember2()->isClassDeclaration();
    if (cd->objc.objc)
        if (!cd->objc.meta) // but check that it hasn't already been done
            ad = cd->objc.metaclass;
}

ControlFlow objc_FuncDeclaration_isVirtual(FuncDeclaration *self, Dsymbol *p, bool &result)
{
    if (self->linkage == LINKobjc)
    {
        // * final member functions are kept virtual with Objective-C linkage
        //   because the Objective-C runtime always use dynamic dispatch.
        // * static member functions are kept virtual too, as they represent
        //   methods of the metaclass.
        result = self->isMember() &&
            !(self->protection == PROTprivate || self->protection == PROTpackage) &&
            p->isClassDeclaration();

        return CFreturn;
    }

    return CFnone;
}

bool objc_FuncDeclaration_objcPreinitInvariant(FuncDeclaration *self)
{
    return self->ident != Id::_dobjc_preinit && self->ident != Id::_dobjc_invariant;
}

// MARK: toObjFile

void objc_FuncDeclaration_toObjFile_extraArgument(FuncDeclaration *self, size_t &pi)
{
    if (self->objc.selector)
        pi += 1; // Extra arument for Objective-C selector
}

void objc_FuncDeclaration_toObjFile_selfCmd(FuncDeclaration *self, Symbol **params, size_t &pi)
{
    if (self->objc.selector)
    {
        // Need to add Objective-C self and _cmd arguments as last/first parameters
        //        error("Objective-C method ABI not implemented yet.");
        assert(self->objc.vcmd);
        Symbol *sobjccmd = toSymbol(self->objc.vcmd);

        // sthis becomes first parameter
        memmove(params + 1, params, pi * sizeof(params[0]));
        params[0] = sobjccmd;
        pi += 1;
    }
}

ControlFlow objc_ClassDeclaration_toObjFile(ClassDeclaration *self, bool multiobj)
{
    if (self->objc.objc)
    {
        if (!self->objc.meta)
        {
            ObjcClassDeclaration* objcdecl = ObjcClassDeclaration::create(self);
            objcdecl->toObjFile(multiobj);
            self->objc.classSymbol = objcdecl->symbol;
        }
        return CFreturn; // skip rest of output
    }

    return CFnone;
}

// MARK: implicitConvTo

ControlFlow objc_implicitConvTo_visit_StringExp_Tclass(Type *t, MATCH *result)
{
    ClassDeclaration *cd = ((TypeClass *)t)->sym;
    if (cd->objc.objc && (cd->objc.takesStringLiteral))
    {
        *result = MATCHexact;
        return CFreturn;
    }
    return CFbreak;
}

MATCH objc_implicitConvTo_visit_ObjcSelectorExp(Type *&t, ObjcSelectorExp *e)
{
#if 0
    printf("ObjcSelectorExp::implicitConvTo(this=%s, type=%s, t=%s)\n",
           e->toChars(), e->type->toChars(), t->toChars());
#endif
    MATCH result = e->type->implicitConvTo(t);
    if (result != MATCHnomatch)
        return result;

    // Look for pointers to functions where the functions are overloaded.
    t = t->toBasetype();
    if (e->type->ty == Tobjcselector && e->type->nextOf()->ty == Tfunction &&
        t->ty == Tobjcselector && t->nextOf()->ty == Tfunction)
    {
        if (e->func && e->func->overloadExactMatch(t->nextOf()))
            result = MATCHexact;
    }

    return result;
}

// MARK: castTo

ControlFlow objc_castTo_visit_StringExp_Tclass(Scope *sc, Type *t, Expression *&result, StringExp *e, Type *tb)
{
    if (tb->ty == Tclass)
    {
        // convert to Objective-C NSString literal

        if (e->type->ty != Tclass) // not already converted to a string literal
        {
            if (((TypeClass *)tb)->sym->objc.objc &&
                ((TypeClass *)tb)->sym->objc.takesStringLiteral)
            {
                if (e->committed)
                {
                    e->error("cannot convert string literal to NSString because of explicit character type");
                    result = new ErrorExp();
                    return CFreturn;
                }
                e->type = t;
                e->semantic(sc);
            }
        }
        result = e;
        return CFreturn;
    }
    return CFnone;
}

ControlFlow objc_castTo_visit_StringExp_isSelector(Type *t, Expression *&result, StringExp *e, Type *tb)
{
    // Either a typed selector or a pointer to a struct designated as a
    // selector type
    if (tb->ty == Tobjcselector ||
        (tb->ty == Tpointer && tb->nextOf()->toBasetype()->ty == Tstruct &&
         ((TypeStruct *)tb->nextOf()->toBasetype())->sym->objc.isSelector))
    {
        if (e->committed)
        {
            e->error("cannot convert string literal to Objective-C selector because of explicit character type");
            result = new ErrorExp();
            return CFreturn;
        }
        Expression *ose = new ObjcSelectorExp(e->loc, (char *)e->string);
        ose->type = t;
        result = ose;
        return CFreturn;
    }
    return CFnone;
}

ControlFlow objc_castTo_visit_SymOffExp_Tobjcselector(Scope *sc, Expression *&result, SymOffExp *e, FuncDeclaration *f)
{
    if (f->objc.selector && f->linkage == LINKobjc && f->needThis())
    {
        result = new ObjcSelectorExp(e->loc, f);
        result = result->semantic(sc);
    }
    else
    {
        e->error("function %s has no selector", f->toChars());
        result = new ErrorExp();
    }

    return CFreturn;
}

ControlFlow objc_castTo_visit_DelegateExp_Tobjcselector(Type *t, Expression *&result, DelegateExp *e, Type *tb)
{
    static char msg2[] = "cannot form selector due to covariant return type";
    if (e->func)
    {
        FuncDeclaration *f = e->func->overloadExactMatch(tb->nextOf());
        if (f)
        {
            int offset;
            if (f->tintro && f->tintro->nextOf()->isBaseOf(f->type->nextOf(), &offset) && offset)
                e->error("%s", msg2);

            result = new ObjcSelectorExp(e->loc, f);
            result->type = t;
            return CFreturn;
        }
        if (e->func->tintro)
            e->error("%s", msg2);
    }

    return CFnone;
}

ControlFlow objc_castTo_visit_ObjcSelectorExp(Type *t, Expression *&result, ObjcSelectorExp *e)
{
#if 0
    printf("ObjcSelectorExp::castTo(this=%s, type=%s, t=%s)\n",
           e->toChars(), e->type->toChars(), t->toChars());
#endif
    static const char msg[] = "cannot form selector due to covariant return type";

    Type *tb = t->toBasetype();
    Type *typeb = e->type->toBasetype();
    if (tb != typeb)
    {
        // Look for delegates to functions where the functions are overloaded.
        if (typeb->ty == Tobjcselector && typeb->nextOf()->ty == Tfunction &&
            tb->ty == Tobjcselector && tb->nextOf()->ty == Tfunction)
        {
            if (e->func)
            {
                FuncDeclaration *f = e->func->overloadExactMatch(tb->nextOf());
                if (f)
                {
                    int offset;
                    if (f->tintro && f->tintro->nextOf()->isBaseOf(f->type->nextOf(), &offset) && offset)
                        e->error("%s", msg);

                    result = new ObjcSelectorExp(e->loc, f);
                    result->type = t;
                    return CFreturn;
                }
                if (e->func->tintro)
                    e->error("%s", msg);
            }
        }
        return CFvisit;
    }
    else
    {
        int offset;

        if (e->func && e->func->tintro && e->func->tintro->nextOf()->isBaseOf(e->func->type->nextOf(), &offset) && offset)
            e->error("%s", msg);
        result = e->copy();
        result->type = t;
    }

    return CFnone;
}

// MARK: cppmangle

void objc_CppMangleVisitor_visit_TypeObjcSelector(OutBuffer &buf, TypeObjcSelector *t)
{
    buf.writestring("P13objc_selector");
}

ControlFlow objc_ScopeDsymbol_multiplyDefined(Dsymbol *s1, Dsymbol *s2)
{
    bool isMetaclass = s1->isClassDeclaration() && s2->isClassDeclaration() &&
        ((ClassDeclaration *)s1)->objc.meta && ((ClassDeclaration *)s2)->objc.meta;

    return isMetaclass ? CFreturn : CFnone;
}

// MARK: callfunc
void objc_callfunc_setupSelector(elem *ec, FuncDeclaration *fd, elem *esel, Type *t, TypeFunction *&tf, elem *&ethis)
{
    assert(!fd);
    assert(esel);
    assert(t->nextOf()->ty == Tfunction);
    tf = (TypeFunction *)(t->nextOf());
    ethis = ec;
}

void objc_callfunc_setupMethodSelector(Type *tret, FuncDeclaration *fd, Type *t, elem *ehidden, elem *&esel)
{
    if (fd && fd->objc.selector && !esel)
    {
        if (fd->objc.selector->usesVTableDispatch())
        {
            elem* messageReference = el_var(ObjcSymbols::getMessageReference(fd->objc.selector, tret, ehidden != 0));
            esel = addressElem(messageReference, t);
        }

        else
            esel = fd->objc.selector->toElem();
    }
}

void objc_callfunc_setupEp(elem *esel, elem *&ep, int reverse)
{
    if (esel)
    {
        // using objc-style "virtual" call
        // add hidden argument (second to 'this') for selector used by dispatch function
        if (reverse)
            ep = el_param(esel,ep);
        else
            ep = el_param(ep,esel);
    }
}

void objc_callfunc_checkThisForSelector(elem *esel, elem *ethis)
{
    if (esel)
    {
        // All functions with a selector need a this pointer.
        assert(ethis);
    }
}

void objc_callfunc_setupMethodCall(int directcall, elem *&ec, FuncDeclaration *fd, Type *t, elem *&ehidden, elem *&ethis, TypeFunction *tf, Symbol *sfunc)
{
    if (fd->fbody && (!fd->isVirtual() || directcall || fd->isFinal()))
    {
        // make static call
        // this is an optimization that the Objective-C compiler
        // does not make, we do it only if the function to call is
        // defined in D code (has a body)
        ec = el_var(sfunc);
    }
    else if (directcall)
    {
        // call through Objective-C runtime dispatch
        ec = el_var(ObjcSymbols::getMsgSendSuper(ehidden != 0));

        // need to change this pointer to a pointer to an two-word
        // objc_super struct of the form { this ptr, class ptr }.
        AggregateDeclaration *ad = fd->isThis();
        ClassDeclaration *cd = ad->isClassDeclaration();
        assert(cd /* call to objc_msgSendSuper with no class delcaration */);

        // FIXME: faking delegate type and objc_super types
        elem *eclassref = el_var(ObjcSymbols::getClassReference(cd));
        elem *esuper = el_pair(TYdelegate, ethis, eclassref);

        ethis = addressElem(esuper, t); // get a pointer to our objc_super struct
    }
    else
    {
        // make objc-style "virtual" call using dispatch function
        assert(ethis);
        Type *tret = tf->next;

        if (fd->objc.selector->usesVTableDispatch())
            ec = el_var(ObjcSymbols::getMsgSendFixup(tret, ehidden != 0));
        else
            ec = el_var(ObjcSymbols::getMsgSend(tret, ehidden != 0));
    }
}

void objc_callfunc_setupSelectorCall(elem *&ec, elem *ehidden, elem *ethis, TypeFunction *tf)
{
    // make objc-style "virtual" call using dispatch function
    assert(ethis);
    Type *tret = tf->next;
    ec = el_var(ObjcSymbols::getMsgSend(tret, ehidden != 0));
}

// MARK: toElem

void objc_toElem_visit_StringExp_Tclass(StringExp *se, elem *&e)
{
    Symbol *si = ObjcSymbols::getStringLiteral(se->string, se->len, se->sz);
    e = el_ptr(si);
}

void objc_toElem_visit_NewExp_Tclass(IRState *irs, NewExp *ne, Type *&ectype, TypeClass *tclass, ClassDeclaration *cd, elem *&ex, elem *&ey, elem *&ez)
{
    elem *ei;
    Symbol *si;

    if (ne->onstack)
        ne->error("cannot allocate Objective-C class on the stack");

    if (ne->objcalloc)
    {
        // Call allocator func with class reference
        ex = el_var(ObjcSymbols::getClassReference(cd));
        ex = callfunc(ne->loc, irs, 0, ne->type, ex, ne->objcalloc->type,
                      ne->objcalloc, ne->objcalloc->type, NULL, ne->newargs, NULL);
    }
    else
    {
        ne->error("Cannot allocate Objective-C class, missing 'alloc' function.");
        exit(-1);
    }

    // FIXME: skipping initialization (actually, all fields will be zeros)
    // Need to assign each non-zero field separately.

    //si = tclass->sym->toInitializer();
    //ei = el_var(si);

    if (cd->isNested())
    {
        ey = el_same(&ex);
        ez = el_copytree(ey);
    }
    else if (ne->member)
        ez = el_same(&ex);

    //ex = el_una(OPind, TYstruct, ex);
    //ex = el_bin(OPstreq, TYnptr, ex, ei);
    //ex->Enumbytes = cd->size(loc);
    //ex = el_una(OPaddr, TYnptr, ex);
    ectype = tclass;
}

bool objc_toElem_visit_NewExp_Tclass_isDirectCall(bool isObjc)
{
#if DMD_OBJC
    // Call Objective-C constructor (not a direct call)
    return !isObjc;
#else
    // Call constructor
    return true;
#endif
}

void objc_toElem_visit_AssertExp_callInvariant(symbol *&ts, elem *&einv, Type *t1)
{
    ts = symbol_genauto(Type_toCtype(t1));
    // Call Objective-C invariant
    einv = el_bin(OPcall, TYvoid, el_var(rtlsym[RTLSYM_DINVARIANT_OBJC]), el_var(ts));
}

void objc_toElem_visit_DotVarExp_nonFragileAbiOffset(VarDeclaration *v, Type *tb1, elem *&offset)
{
    if (global.params.isObjcNonFragileAbi && tb1->ty == Tclass)
    {
        ClassDeclaration* cls = ((TypeClass*) tb1)->sym;
        if (cls->objc.objc)
        {
            NonFragileAbiObjcClassDeclaration objcClass(cls);
            offset = el_var(objcClass.getIVarOffset(v));
        }
    }
}

elem * objc_toElem_visit_ObjcSelectorExp(ObjcSelectorExp *ose)
{
    elem *result = NULL;

    if (ose->func)
        result = ose->func->objc.selector->toElem();
    else if (ose->selname)
        result = ObjcSelector::lookup(ose->selname)->toElem();
    else
        assert(0);

    return result;
}

void objc_toElem_visit_CallExp_selector(IRState *irs, CallExp *ce, elem *&ec, elem *&esel)
{
    assert(ce->argument0);
    ec = toElem(ce->argument0, irs);
    esel = toElem(ce->e1, irs);
}

ControlFlow objc_toElem_visit_CastExp_Tclass_fromObjc(int &rtl, ClassDeclaration *cdfrom, ClassDeclaration *cdto)
{
    if (cdto->objc.objc)
    {   // casting from objc type to objc type, use objc function
        if (cdto->isInterfaceDeclaration())
            rtl = RTLSYM_INTERFACE_CAST_OBJC;
        else if (cdfrom->objc.objc)
            rtl = RTLSYM_DYNAMIC_CAST_OBJC;

        return CFnone;
    }
    else
    {
        // casting from objc type to non-objc type, always null
        return CFgoto;
    }

}

ControlFlow objc_toElem_visit_CastExp_Tclass_toObjc()
{
    // casting from non-objc type to objc type, always null
    return CFgoto;
}

void objc_toElem_visit_CastExp_Tclass_fromObjcToObjcInterface(int &rtl)
{
    rtl = RTLSYM_INTERFACE_CAST_OBJC;
}

void objc_toElem_visit_CastExp_Tclass_assertNoOffset(int offset, ClassDeclaration *cdfrom)
{
    if (cdfrom->objc.objc)
        assert(offset == 0); // no offset for Objective-C objects/interfaces
}

ControlFlow objc_toElem_visit_CastExp_Tclass_toObjcCall(elem *&e, int rtl, ClassDeclaration *cdto)
{
    if (cdto->objc.objc)
    {
        elem *esym;
        if (cdto->isInterfaceDeclaration())
            esym = el_ptr(ObjcSymbols::getProtocolSymbol(cdto));
        else
            esym = el_var(ObjcSymbols::getClassReference(cdto));

        elem *ep = el_param(esym, e);
        e = el_bin(OPcall, TYnptr, el_var(rtlsym[rtl]), ep);
        return CFgoto;
    }

    return CFnone;
}

elem *objc_toElem_visit_ObjcDotClassExp(IRState *irs, ObjcDotClassExp *odce)
{
    elem *e = toElem(odce->e1, irs);
    if (!odce->noop)
    {
        TypeFunction *tf = new TypeFunction(NULL, odce->type, 0, LINKobjc);
        FuncDeclaration *fd = new FuncDeclaration(Loc(), Loc(), NULL, STCstatic, tf);
        fd->protection = PROTpublic;
        fd->linkage = LINKobjc;
        fd->objc.selector = ObjcSelector::lookup("class", 5, 0);

        Expression *ef = new VarExp(Loc(), fd);
        Expression *ec = new CallExp(odce->loc, ef);
        e = toElem(ec, irs);
    }
    return e;
}

elem *objc_toElem_visit_ObjcClassRefExp(ObjcClassRefExp *ocre)
{
    return el_var(ObjcSymbols::getClassReference(ocre->cdecl));
}

elem *objc_toElem_visit_ObjcProtocolOfExp(ObjcProtocolOfExp *e)
{
    return el_ptr(ObjcSymbols::getProtocolSymbol(e->idecl));
}

// MARK: getRightThis

ControlFlow objc_getRightThis(AggregateDeclaration *ad, Expression *&e1, Declaration *var)
{
    ControlFlow controlFlow = CFnone;

    if (e1->op == TOKobjcclsref)
    {
        // We already have an Objective-C class reference, just use that as 'this'.
        controlFlow = CFgoto;
    }
    else if (ad &&
             ad->isClassDeclaration() && ((ClassDeclaration *)ad)->objc.objc &&
             var->isFuncDeclaration() && ((FuncDeclaration *)var)->isStatic() &&
             ((FuncDeclaration *)var)->objc.selector)
    {
        // Create class reference from the class declaration
        e1 = new ObjcClassRefExp(e1->loc, (ClassDeclaration *)ad);
        controlFlow = CFgoto;
    }

    return controlFlow;
}

// MARK: Module::genobjfile

void objc_Module_genobjfile_initSymbols()
{
    ObjcSymbols::init();
}

// MARK: toCBuffer

void objc_toCBuffer_visit_ObjcSelectorExp(OutBuffer *buf, ObjcSelectorExp *e)
{
    buf->writeByte('&');
    if (e->func)
        buf->writestring(e->func->toChars());
    else
        buf->writestring(e->selname);
}

void objc_toCBuffer_visit_ObjcDotClassExp(OutBuffer *buf, HdrGenState *hgs, ObjcDotClassExp *e)
{
    toCBuffer(e->e1, buf, hgs);
    buf->writestring(".class");
}

void objc_toCBuffer_visit_ObjcClassRefExp(OutBuffer *buf, ObjcClassRefExp *e)
{
    buf->writestring(e->cdecl->objc.ident->string);
    buf->writestring(".class");
}

void objc_toCBuffer_visit_ObjcProtocolOfExp(OutBuffer *buf, HdrGenState *hgs, ObjcProtocolOfExp *e)
{
    toCBuffer(e->e1, buf, hgs);
    buf->writestring(".protocolof");
}

// MARK: inline

void objc_inline_visit_ObjcSelectorExp(int &cost)
{
    cost = COST_MAX;
}

// MARK: interpret

void objc_interpret_visit_ObjcSelectorExp(ObjcSelectorExp *e, Expression *&result)
{
#if LOG
    printf("ObjcSelectorExp::interpret() %s\n", e->toChars());
#endif
    result = e;
}

// MARK: Type::init

void objc_Type_init(unsigned char sizeTy[TMAX])
{
    sizeTy[Tobjcselector] = sizeof(TypeObjcSelector);
}

// MARK: dotExp

void objc_Type_dotExp_TOKdotvar_setReceiver(ClassDeclaration *&receiver, DotVarExp *dv)
{
    Type* baseType = dv->e1->type->toBasetype();
    if (baseType && baseType->ty == Tclass)
        receiver = ((TypeClass*) baseType)->sym;
}

void objc_Type_dotExp_TOKvar_setReceiver(VarDeclaration *v, ClassDeclaration *&receiver)
{
    if (Dsymbol* parent = v->toParent())
        receiver = parent->isClassDeclaration();
}

void objc_Type_dotExp_offsetof(Type *self, Expression *e, ClassDeclaration *receiver)
{
    if (receiver && receiver->objc.objc)
        self->error(e->loc, ".offsetof (%s) is not available for members of Objective-C classes (%s)", e->toChars(), receiver->toChars());
}

void objc_TypeClass_dotExp_tupleof(TypeClass *self, Expression *e)
{
    if (self->sym->objc.objc)
        self->error(e->loc, ".tupleof (%s) is not available for Objective-C classes (%s)", e->toChars(), self->sym->toChars());
}

ControlFlow objc_TypeClass_dotExp_protocolof(Scope *sc, Expression *&e, Identifier *ident)
{
    if (ident == Id::protocolof)
    {
        e = new ObjcProtocolOfExp(e->loc, e);
        e = e->semantic(sc);
        return CFreturn;
    }

    return CFnone;
}

void objc_TypeClass_dotExp_TOKtype(TypeClass *self, Scope *sc, Expression *&e, Declaration *d)
{
    // Objective-C class methods uses the class object as 'this'
    DotVarExp *de = new DotVarExp(e->loc, new ObjcClassRefExp(e->loc, self->sym), d);
    e = de->semantic(sc);
}

// MARK: Expression_optimize
void objc_Expression_optimize_visit_CallExp_Tobjcselector(Type *&t1)
{
    if (t1->ty == Tobjcselector)
        t1 = t1->nextOf();
}

// MARK: parse

void objc_Parser_parseCtor_selector(Parser *self, TemplateParameters *tpl, Parameters *parameters, CtorDeclaration *f)
{
    f->objc.selector = objc_parseSelector(self);
    if (f->objc.selector)
    {
        if (tpl)
            self->error("constructor template cannot have an Objective-C selector attached");
        if (f->objc.selector->paramCount != parameters->dim)
            self->error("number of colons in Objective-C selector must match the number of parameters");
    }
}

void objc_Parser_parseDtor(Parser *self, DtorDeclaration *f)
{
    f->objc.selector = objc_parseSelector(self);
}

void objc_Parser_parseBasicType2_selector(Type *&t, TypeFunction *tf)
{
    tf->linkage = LINKobjc; // force Objective-C linkage
    t = new TypeObjcSelector(tf);
}

void objc_Parser_parseDeclarations_Tobjcselector(Type *&t, LINK &link)
{
    if (t->ty == Tobjcselector)
        link = LINKobjc; // force Objective-C linkage
}

void objc_Parser_parseDeclarations_Tfunction(Parser *self, Type *t, TemplateParameters *tpl, FuncDeclaration *f)
{
    f->objc.selector = objc_parseSelector(self);
    if (f->objc.selector)
    {
        TypeFunction *tf = (TypeFunction *)t;
        if (tpl)
            self->error("function template cannot have an Objective-C selector attached");
        if (f->objc.selector->paramCount != tf->parameters->dim)
            self->error("number of colons in Objective-C selector must match number of parameters");
    }
}

/*****************************************
 * Parse Objective-C selector name enclosed in brackets. Such as:
 *   [setObject:forKey:otherArgs::]
 * Return NULL when no bracket found.
 */

ObjcSelector *objc_parseSelector(Parser *self)
{
    if (self->token.value != TOKlbracket)
        return NULL; // no selector

    ObjcSelectorBuilder selBuilder;
    self->nextToken();
    while (1)
    {
        switch (self->token.value)
        {
            case TOKidentifier:
            Lcaseident:
                selBuilder.addIdentifier(self->token.ident);
                break;
            case TOKcolon:
                selBuilder.addColon();
                break;
            case TOKrbracket:
                goto Lendloop;
            default:
                // special case to allow D keywords in Objective-C selector names
                if (self->token.ident)
                    goto Lcaseident;
                goto Lparseerror;
        }
        self->nextToken();
    }
Lendloop:
    self->nextToken();
    if (!selBuilder.isValid())
    {
        self->error("illegal Objective-C selector name");
        return NULL;

    }
    return ObjcSelector::lookup(&selBuilder);

Lparseerror:
    error("illegal Objective-C selector name");
    // exit bracket ignoring content
    while (self->token.value != TOKrbracket && self->token.value != TOKeof)
        self->nextToken();
    self->nextToken();
    return NULL;
}

ControlFlow objc_Parser_parsePostExp_TOKclass(Parser *self, Expression *&e, Loc loc)
{
    e = new ObjcDotClassExp(loc, e);
    self->nextToken();
    return CFcontinue;
}

// MARK: tryMain

void objc_tryMain_dObjc()
{
    VersionCondition::addPredefinedGlobalIdent("D_ObjC");

    if (global.params.isOSX && global.params.is64bit) // && isArm
    {
        global.params.isObjcNonFragileAbi = 1;
        VersionCondition::addPredefinedGlobalIdent("D_ObjCNonFragileABI");
    }
}

void objc_tryMain_init()
{
    ObjcSymbols::init();
    ObjcSelector::init();
}

// MARK: callSideEffectLevel

void objc_callSideEffectLevel_Tobjcselector(Type *t, TypeFunction *&tf)
{
    tf = (TypeFunction *)((TypeDelegate *)t)->next;
}

// MARK: lambdaHasSideEffect

void objc_lambdaHasSideEffect_TOKcall_Tobjcselector(Type *&t)
{
    t = ((TypeObjcSelector *)t)->next;
}

// MARK: Type_toCtype

void objc_Type_toCtype_visit_TypeObjcSelector(TypeObjcSelector *t)
{
    type *tn;

    //printf("TypePointer::toCtype() %s\n", t->toChars());
    if (t->ctype)
        return;

    if (1 || global.params.symdebug)
    {   /* Need to always do this, otherwise C++ name mangling
         * goes awry.
         */
        t->ctype = type_alloc(TYnptr);
        tn = tschar; // expose selector as a char*
        t->ctype->Tnext = tn;
        tn->Tcount++;
    }
    else
        t->ctype = type_fake(totym(t));
    t->ctype->Tcount++;
}

// MARK: Module::genmoduleinfo

void objc_Module_genmoduleinfo_classes(Module *self)
{
    // generate the list of objc classes and categories in this module
    ClassDeclarations objccls;
    ClassDeclarations objccat;
    for (int i = 0; i < self->members->dim; i++)
    {
        Dsymbol *member = self->members->tdata()[i];
        member->addObjcSymbols(&objccls, &objccat);
    }
    // only emit objc module info for modules with Objective-C symbols
    if (objccls.dim || objccat.dim || ObjcSymbols::hassymbols)
        ObjcSymbols::getModuleInfo(&objccls, &objccat);
}

// MARK: TypeInfo_toDt

void objc_TypeInfo_toDt_visit_TypeInfoObjcSelectorDeclaration(dt_t **pdt, TypeInfoObjcSelectorDeclaration *d)
{
    //printf("TypeInfoObjcSelectorDeclaration::toDt()\n");
    dtxoff(pdt, Type::typeinfodelegate->toVtblSymbol(), 0); // vtbl for TypeInfo_ObjcSelector
    dtsize_t(pdt, 0);                        // monitor

    assert(d->tinfo->ty == Tobjcselector);

    TypeObjcSelector *tc = (TypeObjcSelector *)d->tinfo;

    tc->next->nextOf()->getTypeInfo(NULL);
    dtxoff(pdt, toSymbol(tc->next->nextOf()->vtinfo), 0); // TypeInfo for selector return value
}