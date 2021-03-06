
// Compiler implementation of the D programming language
// Copyright (c) 1999-2013 by Digital Mars
// All Rights Reserved
// written by Walter Bright
// http://www.digitalmars.com
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.

#include <stdio.h>
#include <assert.h>

#include "mars.h"
#include "init.h"
#include "expression.h"
#include "statement.h"
#include "identifier.h"
#include "declaration.h"
#include "aggregate.h"
#include "scope.h"
#include "mtype.h"
#include "hdrgen.h"
#include "template.h"
#include "id.h"

/********************************** Initializer *******************************/

Initializer::Initializer(Loc loc)
{
    this->loc = loc;
}

Initializer *Initializer::syntaxCopy()
{
    return this;
}

Initializer *Initializer::semantic(Scope *sc, Type *t, NeedInterpret needInterpret)
{
    return this;
}

Type *Initializer::inferType(Scope *sc)
{
    error(loc, "cannot infer type from initializer");
    return Type::terror;
}

Initializers *Initializer::arraySyntaxCopy(Initializers *ai)
{   Initializers *a = NULL;

    if (ai)
    {
        a = new Initializers();
        a->setDim(ai->dim);
        for (size_t i = 0; i < a->dim; i++)
        {   Initializer *e = (*ai)[i];

            e = e->syntaxCopy();
            (*a)[i] = e;
        }
    }
    return a;
}

char *Initializer::toChars()
{
    HdrGenState hgs;

    OutBuffer buf;
    toCBuffer(&buf, &hgs);
    buf.writebyte(0);
    return buf.extractData();
}

/********************************** ErrorInitializer ***************************/

ErrorInitializer::ErrorInitializer()
    : Initializer(Loc())
{
}


Initializer *ErrorInitializer::syntaxCopy()
{
    return this;
}


Initializer *ErrorInitializer::semantic(Scope *sc, Type *t, NeedInterpret needInterpret)
{
    //printf("ErrorInitializer::semantic(t = %p)\n", t);
    return this;
}


Expression *ErrorInitializer::toExpression(Type *t)
{
    return new ErrorExp();
}


void ErrorInitializer::toCBuffer(OutBuffer *buf, HdrGenState *hgs)
{
    buf->writestring("__error__");
}


/********************************** VoidInitializer ***************************/

VoidInitializer::VoidInitializer(Loc loc)
    : Initializer(loc)
{
    type = NULL;
}


Initializer *VoidInitializer::syntaxCopy()
{
    return new VoidInitializer(loc);
}


Initializer *VoidInitializer::semantic(Scope *sc, Type *t, NeedInterpret needInterpret)
{
    //printf("VoidInitializer::semantic(t = %p)\n", t);
    type = t;
    return this;
}


Expression *VoidInitializer::toExpression(Type *t)
{
    return NULL;
}


void VoidInitializer::toCBuffer(OutBuffer *buf, HdrGenState *hgs)
{
    buf->writestring("void");
}


/********************************** StructInitializer *************************/

StructInitializer::StructInitializer(Loc loc)
    : Initializer(loc)
{
}

Initializer *StructInitializer::syntaxCopy()
{
    StructInitializer *ai = new StructInitializer(loc);

    assert(field.dim == value.dim);
    ai->field.setDim(field.dim);
    ai->value.setDim(value.dim);
    for (size_t i = 0; i < field.dim; i++)
    {
        ai->field[i] = field[i];

        Initializer *init = value[i];
        init = init->syntaxCopy();
        ai->value[i] = init;
    }
    return ai;
}

void StructInitializer::addInit(Identifier *field, Initializer *value)
{
    //printf("StructInitializer::addInit(field = %p, value = %p)\n", field, value);
    this->field.push(field);
    this->value.push(value);
}

Initializer *StructInitializer::semantic(Scope *sc, Type *t, NeedInterpret needInterpret)
{
    //printf("StructInitializer::semantic(t = %s) %s\n", t->toChars(), toChars());
    t = t->toBasetype();
    if (t->ty == Tsarray && t->nextOf()->toBasetype()->ty == Tstruct)
        t = t->nextOf()->toBasetype();
    if (t->ty == Tstruct)
    {
        StructDeclaration *sd = ((TypeStruct *)t)->sym;
        if (sd->ctor)
        {
            error(loc, "%s %s has constructors, cannot use { initializers }, use %s( initializers ) instead",
                sd->kind(), sd->toChars(), sd->toChars());
            return new ErrorInitializer();
        }
        sd->size(loc);
        if (sd->sizeok != SIZEOKdone)
            return new ErrorInitializer();
        size_t nfields = sd->fields.dim - sd->isNested();

        //expandTuples for non-identity arguments?

        Expressions *elements = new Expressions();
        elements->setDim(nfields);
        for (size_t i = 0; i < elements->dim; i++)
            (*elements)[i] = NULL;

        // Run semantic for explicitly given initializers
        // TODO: this part is slightly different from StructLiteralExp::semantic.
        bool errors = false;
        for (size_t fieldi = 0, i = 0; i < field.dim; i++)
        {
            if (Identifier *id = field[i])
            {
                Dsymbol *s = sd->search(loc, id, 0);
                if (!s)
                {
                    s = sd->search_correct(id);
                    if (s)
                        error(loc, "'%s' is not a member of '%s', did you mean '%s %s'?",
                              id->toChars(), sd->toChars(), s->kind(), s->toChars());
                    else
                        error(loc, "'%s' is not a member of '%s'", id->toChars(), sd->toChars());
                    return new ErrorInitializer();
                }
                s = s->toAlias();

                // Find out which field index it is
                for (fieldi = 0; 1; fieldi++)
                {
                    if (fieldi >= nfields)
                    {
                        error(loc, "%s.%s is not a per-instance initializable field",
                            sd->toChars(), s->toChars());
                        return new ErrorInitializer();
                    }
                    if (s == sd->fields[fieldi])
                        break;
                }
            }
            else if (fieldi >= nfields)
            {
                error(loc, "too many initializers for %s", sd->toChars());
                return new ErrorInitializer();
            }

            VarDeclaration *vd = sd->fields[fieldi];
            if ((*elements)[fieldi])
            {
                error(loc, "duplicate initializer for field '%s'", vd->toChars());
                errors = true;
                continue;
            }
            for (size_t j = 0; j < nfields; j++)
            {
                VarDeclaration *v2 = sd->fields[j];
                bool overlap = (vd->offset < v2->offset + v2->type->size() &&
                                v2->offset < vd->offset + vd->type->size());
                if (overlap && (*elements)[j])
                {
                    error(loc, "overlapping initialization for field %s and %s",
                        v2->toChars(), vd->toChars());
                    errors = true;
                    continue;
                }
            }

            assert(sc);
            Initializer *iz = value[i];
            iz = iz->semantic(sc, vd->type->addMod(t->mod), needInterpret);
            Expression *ex = iz->toExpression();
            if (ex->op == TOKerror)
            {
                errors = true;
                continue;
            }
            value[i] = iz;
            (*elements)[fieldi] = ex;
            ++fieldi;
        }
        if (errors)
            return new ErrorInitializer();

        StructLiteralExp *sle = new StructLiteralExp(loc, sd, elements, t);
        Expression *e = sle->fill(false);
        if (e->op == TOKerror)
            return new ErrorInitializer();

        e->type = t;

        ExpInitializer *ie = new ExpInitializer(loc, e);
        return ie->semantic(sc, t, needInterpret);
    }
    else if (t->ty == Tdelegate && value.dim == 0)
    {
        /* Rewrite as empty delegate literal { }
         */
        Parameters *arguments = new Parameters;
        Type *tf = new TypeFunction(arguments, NULL, 0, LINKd);
        FuncLiteralDeclaration *fd = new FuncLiteralDeclaration(loc, Loc(), tf, TOKdelegate, NULL);
        fd->fbody = new CompoundStatement(loc, new Statements());
        fd->endloc = loc;
        Expression *e = new FuncExp(loc, fd);
        ExpInitializer *ie = new ExpInitializer(loc, e);
        return ie->semantic(sc, t, needInterpret);
    }

    error(loc, "a struct is not a valid initializer for a %s", t->toChars());
    return new ErrorInitializer();
}

/***************************************
 * This works by transforming a struct initializer into
 * a struct literal. In the future, the two should be the
 * same thing.
 */
Expression *StructInitializer::toExpression(Type *t)
{
    // cannot convert to an expression without target 'ad'
    return NULL;
}

void StructInitializer::toCBuffer(OutBuffer *buf, HdrGenState *hgs)
{
    //printf("StructInitializer::toCBuffer()\n");
    buf->writebyte('{');
    for (size_t i = 0; i < field.dim; i++)
    {
        if (i > 0)
            buf->writestring(", ");
        Identifier *id = field[i];
        if (id)
        {
            buf->writestring(id->toChars());
            buf->writebyte(':');
        }
        Initializer *iz = value[i];
        if (iz)
            iz->toCBuffer(buf, hgs);
    }
    buf->writebyte('}');
}

/********************************** ArrayInitializer ************************************/

ArrayInitializer::ArrayInitializer(Loc loc)
    : Initializer(loc)
{
    dim = 0;
    type = NULL;
    sem = 0;
}

Initializer *ArrayInitializer::syntaxCopy()
{
    //printf("ArrayInitializer::syntaxCopy()\n");

    ArrayInitializer *ai = new ArrayInitializer(loc);

    assert(index.dim == value.dim);
    ai->index.setDim(index.dim);
    ai->value.setDim(value.dim);
    for (size_t i = 0; i < ai->value.dim; i++)
    {   Expression *e = index[i];
        if (e)
            e = e->syntaxCopy();
        ai->index[i] = e;

        Initializer *init = value[i];
        init = init->syntaxCopy();
        ai->value[i] = init;
    }
    return ai;
}

void ArrayInitializer::addInit(Expression *index, Initializer *value)
{
    this->index.push(index);
    this->value.push(value);
    dim = 0;
    type = NULL;
}

Initializer *ArrayInitializer::semantic(Scope *sc, Type *t, NeedInterpret needInterpret)
{
    size_t length;
    const unsigned amax = 0x80000000;
    bool errors = false;

    //printf("ArrayInitializer::semantic(%s)\n", t->toChars());
    if (sem)                            // if semantic() already run
        return this;
    sem = 1;
    type = t;
    Initializer *aa = NULL;
    t = t->toBasetype();
    switch (t->ty)
    {
        case Tpointer:
        case Tsarray:
        case Tarray:
            break;

        case Tvector:
            t = ((TypeVector *)t)->basetype;
            break;

        case Taarray:
            // was actually an associative array literal
            aa = new ExpInitializer(loc, toAssocArrayLiteral());
            return aa->semantic(sc, t, needInterpret);

        default:
            error(loc, "cannot use array to initialize %s", type->toChars());
            goto Lerr;
    }

    length = 0;
    for (size_t i = 0; i < index.dim; i++)
    {
        Expression *idx = index[i];
        if (idx)
        {
            sc = sc->startCTFE();
            idx = idx->semantic(sc);
            sc = sc->endCTFE();
            idx = idx->ctfeInterpret();
            index[i] = idx;
            length = (size_t)idx->toInteger();
            if (idx->op == TOKerror)
                errors = true;
        }

        Initializer *val = value[i];
        ExpInitializer *ei = val->isExpInitializer();
        if (ei && !idx)
            ei->expandTuples = 1;
        val = val->semantic(sc, t->nextOf(), needInterpret);
        if (val->isErrorInitializer())
            errors = true;

        ei = val->isExpInitializer();
        // found a tuple, expand it
        if (ei && ei->exp->op == TOKtuple)
        {
            TupleExp *te = (TupleExp *)ei->exp;
            index.remove(i);
            value.remove(i);

            for (size_t j = 0; j < te->exps->dim; ++j)
            {
                Expression *e = (*te->exps)[j];
                index.insert(i + j, (Expression *)NULL);
                value.insert(i + j, new ExpInitializer(e->loc, e));
            }
            i--;
            continue;
        }
        else
        {
            value[i] = val;
        }

        length++;
        if (length == 0)
        {   error(loc, "array dimension overflow");
            goto Lerr;
        }
        if (length > dim)
            dim = length;
    }
    if (t->ty == Tsarray)
    {
        dinteger_t edim = ((TypeSArray *)t)->dim->toInteger();
        if (dim > edim)
        {
            error(loc, "array initializer has %u elements, but array length is %lld", dim, edim);
            goto Lerr;
        }
    }
    if (errors)
        goto Lerr;

    if ((uinteger_t) dim * t->nextOf()->size() >= amax)
    {   error(loc, "array dimension %u exceeds max of %u", (unsigned) dim, (unsigned)(amax / t->nextOf()->size()));
        goto Lerr;
    }
    return this;

Lerr:
    return new ErrorInitializer();
}

/********************************
 * If possible, convert array initializer to array literal.
 * Otherwise return NULL.
 */

Expression *ArrayInitializer::toExpression(Type *tx)
{
    //printf("ArrayInitializer::toExpression(), dim = %d\n", dim);
    //static int i; if (++i == 2) halt();

    Expressions *elements;
    size_t edim;
    Type *t = NULL;
    if (type)
    {
        if (type == Type::terror)
            return new ErrorExp();

        t = type->toBasetype();
        switch (t->ty)
        {
           case Tsarray:
               edim = (size_t)((TypeSArray *)t)->dim->toInteger();
               break;

           case Tpointer:
           case Tarray:
               edim = dim;
               break;

           default:
               assert(0);
        }
    }
    else
    {
        edim = value.dim;
        for (size_t i = 0, j = 0; i < value.dim; i++, j++)
        {
            if (index[i])
            {
                if (index[i]->op == TOKint64)
                    j = (size_t)index[i]->toInteger();
                else
                    goto Lno;
            }
            if (j >= edim)
                edim = j + 1;
        }
    }

    elements = new Expressions();
    elements->setDim(edim);
    elements->zero();
    for (size_t i = 0, j = 0; i < value.dim; i++, j++)
    {
        if (index[i])
            j = (size_t)(index[i])->toInteger();
        assert(j < edim);
        Initializer *iz = value[i];
        if (!iz)
            goto Lno;
        Expression *ex = iz->toExpression();
        if (!ex)
        {
            goto Lno;
        }
        (*elements)[j] = ex;
    }

    /* Fill in any missing elements with the default initializer
     */
    {
    Expression *init = NULL;
    for (size_t i = 0; i < edim; i++)
    {
        if (!(*elements)[i])
        {
            if (!type)
                goto Lno;
            if (!init)
                init = ((TypeNext *)t)->next->defaultInit();
            (*elements)[i] = init;
        }
    }

    for (size_t i = 0; i < edim; i++)
    {   Expression *e = (*elements)[i];
        if (e->op == TOKerror)
            return e;
    }

    Expression *e = new ArrayLiteralExp(loc, elements);
    e->type = type;
    return e;
    }

Lno:
    return NULL;
}


/********************************
 * If possible, convert array initializer to associative array initializer.
 */

Expression *ArrayInitializer::toAssocArrayLiteral()
{
    Expression *e;

    //printf("ArrayInitializer::toAssocArrayInitializer()\n");
    //static int i; if (++i == 2) halt();
    Expressions *keys = new Expressions();
    keys->setDim(value.dim);
    Expressions *values = new Expressions();
    values->setDim(value.dim);

    for (size_t i = 0; i < value.dim; i++)
    {
        e = index[i];
        if (!e)
            goto Lno;
        (*keys)[i] = e;

        Initializer *iz = value[i];
        if (!iz)
            goto Lno;
        e = iz->toExpression();
        if (!e)
            goto Lno;
        (*values)[i] = e;
    }
    e = new AssocArrayLiteralExp(loc, keys, values);
    return e;

Lno:
    delete keys;
    delete values;
    error(loc, "not an associative array initializer");
    return new ErrorExp();
}

int ArrayInitializer::isAssociativeArray()
{
    for (size_t i = 0; i < value.dim; i++)
    {
        if (index[i])
            return 1;
    }
    return 0;
}

Type *ArrayInitializer::inferType(Scope *sc)
{
    //printf("ArrayInitializer::inferType() %s\n", toChars());
    assert(0);
    return NULL;
#if 0
    type = Type::terror;
    for (size_t i = 0; i < value.dim; i++)
    {
        if (index.data[i])
            goto Laa;
    }
    for (size_t i = 0; i < value.dim; i++)
    {
        Initializer *iz = (Initializer *)value.data[i];
        if (iz)
        {   Type *t = iz->inferType(sc);
            if (i == 0)
            {   /* BUG: This gets the type from the first element.
                 * Fix to use all the elements to figure out the type.
                 */
                t = new TypeSArray(t, new IntegerExp(value.dim));
                t = t->semantic(loc, sc);
                type = t;
            }
        }
    }
    return type;

Laa:
    /* It's possibly an associative array initializer.
     * BUG: inferring type from first member.
     */
    Initializer *iz = (Initializer *)value.data[0];
    Expression *indexinit = (Expression *)index.data[0];
    if (iz && indexinit)
    {   Type *t = iz->inferType(sc);
        indexinit = indexinit->semantic(sc);
        Type *indext = indexinit->type;
        t = new TypeAArray(t, indext);
        type = t->semantic(loc, sc);
    }
    else
        error(loc, "cannot infer type from this array initializer");
    return type;
#endif
}


void ArrayInitializer::toCBuffer(OutBuffer *buf, HdrGenState *hgs)
{
    buf->writebyte('[');
    for (size_t i = 0; i < index.dim; i++)
    {
        if (i > 0)
            buf->writestring(", ");
        Expression *ex = index[i];
        if (ex)
        {
            ex->toCBuffer(buf, hgs);
            buf->writebyte(':');
        }
        Initializer *iz = value[i];
        if (iz)
            iz->toCBuffer(buf, hgs);
    }
    buf->writebyte(']');
}


/********************************** ExpInitializer ************************************/

ExpInitializer::ExpInitializer(Loc loc, Expression *exp)
    : Initializer(loc)
{
    this->exp = exp;
    this->expandTuples = 0;
}

Initializer *ExpInitializer::syntaxCopy()
{
    return new ExpInitializer(loc, exp->syntaxCopy());
}

bool arrayHasNonConstPointers(Expressions *elems);

bool hasNonConstPointers(Expression *e)
{
    if (e->type->ty == Terror)
        return false;

    if (e->op == TOKnull)
        return false;
    if (e->op == TOKstructliteral)
    {
        StructLiteralExp *se = (StructLiteralExp *)e;
        return arrayHasNonConstPointers(se->elements);
    }
    if (e->op == TOKarrayliteral)
    {
        if (!e->type->nextOf()->hasPointers())
            return false;
        ArrayLiteralExp *ae = (ArrayLiteralExp *)e;
        return arrayHasNonConstPointers(ae->elements);
    }
    if (e->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *ae = (AssocArrayLiteralExp *)e;
        if (ae->type->nextOf()->hasPointers() &&
            arrayHasNonConstPointers(ae->values))
                return true;
        if (((TypeAArray *)ae->type)->index->hasPointers())
            return arrayHasNonConstPointers(ae->keys);
        return false;
    }
    if(e->op == TOKaddress)
    {
        AddrExp *ae = (AddrExp *)e;
        if (ae->e1->op == TOKstructliteral)
        {
            StructLiteralExp *se = (StructLiteralExp *)ae->e1;
            if (!(se->stageflags & stageSearchPointers))
            {
                int old = se->stageflags;
                se->stageflags |= stageSearchPointers;
                bool ret = arrayHasNonConstPointers(se->elements);
                se->stageflags = old;
                return ret;
            }
            else
            {
                return false;
            }
        }
        return true;
    }
    if (e->type->ty== Tpointer && e->type->nextOf()->ty != Tfunction)
    {
        if (e->op == TOKsymoff) // address of a global is OK
            return false;
        if (e->op == TOKint64)  // cast(void *)int is OK
            return false;
        if (e->op == TOKstring) // "abc".ptr is OK
            return false;
        return true;
    }
    return false;
}

bool arrayHasNonConstPointers(Expressions *elems)
{
    for (size_t i = 0; i < elems->dim; i++)
    {   Expression *e = (*elems)[i];
        if (e && hasNonConstPointers(e))
            return true;
    }
    return false;
}



Initializer *ExpInitializer::semantic(Scope *sc, Type *t, NeedInterpret needInterpret)
{
    //printf("ExpInitializer::semantic(%s), type = %s\n", exp->toChars(), t->toChars());
    if (needInterpret) sc = sc->startCTFE();
    exp = exp->semantic(sc);
    exp = resolveProperties(sc, exp);
    if (needInterpret) sc = sc->endCTFE();
    if (exp->op == TOKerror)
        return new ErrorInitializer();

    int olderrors = global.errors;
    if (needInterpret)
        exp = exp->ctfeInterpret();
    else
        exp = exp->optimize(WANTvalue);
    if (!global.gag && olderrors != global.errors)
        return this; // Failed, suppress duplicate error messages

    if (exp->op == TOKtype)
    {
        exp->error("initializer must be an expression, not '%s'", exp->toChars());
        return new ErrorInitializer();
    }

    // Make sure all pointers are constants
    if (needInterpret && hasNonConstPointers(exp))
    {
        exp->error("cannot use non-constant CTFE pointer in an initializer '%s'", exp->toChars());
        return new ErrorInitializer();
    }

    Type *tb = t->toBasetype();
    Type *ti = exp->type->toBasetype();

    if (exp->op == TOKtuple &&
        expandTuples &&
        !exp->implicitConvTo(t))
        return new ExpInitializer(loc, exp);

    /* Look for case of initializing a static array with a too-short
     * string literal, such as:
     *  char[5] foo = "abc";
     * Allow this by doing an explicit cast, which will lengthen the string
     * literal.
     */
    if (exp->op == TOKstring && tb->ty == Tsarray && ti->ty == Tsarray)
    {   StringExp *se = (StringExp *)exp;

        if (!se->committed && se->type->ty == Tsarray &&
            ((TypeSArray *)se->type)->dim->toInteger() <
            ((TypeSArray *)t)->dim->toInteger())
        {
            exp = se->castTo(sc, t);
            goto L1;
        }
    }

    // Look for implicit constructor call
    if (tb->ty == Tstruct &&
        !(ti->ty == Tstruct && tb->toDsymbol(sc) == ti->toDsymbol(sc)) &&
        !exp->implicitConvTo(t))
    {
        StructDeclaration *sd = ((TypeStruct *)tb)->sym;
        if (sd->ctor)
        {   // Rewrite as S().ctor(exp)
            Expression *e;
            e = new StructLiteralExp(loc, sd, NULL);
            e = new DotIdExp(loc, e, Id::ctor);
            e = new CallExp(loc, e, exp);
            e = e->semantic(sc);
            if (needInterpret)
                exp = e->ctfeInterpret();
            else
                exp = e->optimize(WANTvalue);
        }
    }

    // Look for the case of statically initializing an array
    // with a single member.
    if (tb->ty == Tsarray &&
        !tb->nextOf()->equals(ti->toBasetype()->nextOf()) &&
        exp->implicitConvTo(tb->nextOf())
       )
    {
        /* If the variable is not actually used in compile time, array creation is
         * redundant. So delay it until invocation of toExpression() or toDt().
         */
        t = tb->nextOf();
    }

    exp = exp->implicitCastTo(sc, t);
    if (exp->op == TOKerror)
        return this;
L1:
    if (needInterpret)
        exp = exp->ctfeInterpret();
    else
        exp = exp->optimize(WANTvalue);
    //printf("-ExpInitializer::semantic(): "); exp->print();
    return this;
}

Type *ExpInitializer::inferType(Scope *sc)
{
    //printf("ExpInitializer::inferType() %s\n", toChars());
    exp = exp->semantic(sc);
    exp = resolveProperties(sc, exp);
    if (exp->op == TOKimport)
    {   ScopeExp *se = (ScopeExp *)exp;
        TemplateInstance *ti = se->sds->isTemplateInstance();
        if (ti && ti->semanticRun == PASSsemantic && !ti->aliasdecl)
            se->error("cannot infer type from %s %s, possible circular dependency", se->sds->kind(), se->toChars());
        else
            se->error("cannot infer type from %s %s", se->sds->kind(), se->toChars());
        return Type::terror;
    }

    // Give error for overloaded function addresses
    if (exp->op == TOKsymoff)
    {
        SymOffExp *se = (SymOffExp *)exp;
        if (se->hasOverloads && !se->var->isFuncDeclaration()->isUnique())
        {
            exp->error("cannot infer type from overloaded function symbol %s", exp->toChars());
            return Type::terror;
        }
    }
    if (exp->op == TOKdelegate)
    {
        DelegateExp *se = (DelegateExp *)exp;
        if (se->hasOverloads &&
            se->func->isFuncDeclaration() &&
            !se->func->isFuncDeclaration()->isUnique())
        {
            exp->error("cannot infer type from overloaded function symbol %s", exp->toChars());
            return Type::terror;
        }
    }
    if (exp->op == TOKaddress)
    {
        AddrExp *ae = (AddrExp *)exp;
        if (ae->e1->op == TOKoverloadset)
        {
            exp->error("cannot infer type from overloaded function symbol %s", exp->toChars());
            return Type::terror;
        }
    }

    Type *t = exp->type;
    if (!t)
        t = Initializer::inferType(sc);
    return t;
}

Expression *ExpInitializer::toExpression(Type *t)
{
    if (t)
    {
        Type *tb = t->toBasetype();
        if (tb->ty == Tsarray && exp->implicitConvTo(tb->nextOf()))
        {
            TypeSArray *tsa = (TypeSArray *)tb;
            size_t d = (size_t)tsa->dim->toInteger();
            Expressions *elements = new Expressions();
            elements->setDim(d);
            for (size_t i = 0; i < d; i++)
                (*elements)[i] = exp;
            ArrayLiteralExp *ae = new ArrayLiteralExp(exp->loc, elements);
            ae->type = t;
            exp = ae;
        }
    }
    return exp;
}


void ExpInitializer::toCBuffer(OutBuffer *buf, HdrGenState *hgs)
{
    exp->toCBuffer(buf, hgs);
}



