/**
 * Performs the semantic3 stage, which deals with function bodies.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Jacob Carlborg)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/semantic3.d, _semantic3.d)
 * Documentation:  https://dlang.org/phobos/dmd_semantic3.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/semantic3.d
 */

module dmd.zero_cost_error_handling;

import dmd.arraytypes;
import dmd.dscope;
import dmd.dtemplate;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.id;
import dmd.identifier;
import dmd.mtype;
import dmd.statement;
import dmd.statementsem;
import dmd.typesem;

Type transformReturnType(FuncDeclaration funcdecl, Scope* sc)
{
    auto tiargs = new Objects;
    tiargs.reserve(funcdecl.throwArgs.length + 1);
    tiargs.push(funcdecl.type.isTypeFunction.next);

    foreach (arg; funcdecl.throwArgs.opSlice)
        tiargs.push(arg);

    auto resultIdentifier = Identifier.idPool("Result");
    auto templateInstance = new TemplateInstance(funcdecl.loc, resultIdentifier, tiargs);
    auto typeInstance = new TypeInstance(funcdecl.loc, templateInstance);

    return typeInstance.typeSemantic(funcdecl.loc, sc);
}

Statement transformThrowStatement(ThrowStatement ts, Scope* sc)
{
    auto tiargs = new Objects;
    tiargs.reserve(sc.func.throwArgs.length + 1);
    auto funcType = sc.func.type.isTypeFunction;
    assert(funcType);
    tiargs.push(funcType.next);

    foreach (arg; sc.func.throwArgs.opSlice)
        tiargs.push(arg);

    auto resultIdentifier = Identifier.idPool("Result");
    auto rootIdentifier = new IdentifierExp(ts.loc, Id.empty);
    auto dt = new DotTemplateInstanceExp(ts.loc, rootIdentifier, resultIdentifier, tiargs);

    auto resultCall = CallExp.create(ts.loc, dt, ts.exp);
    auto rs = new ReturnStatement(ts.loc, resultCall);
    rs.isZeroCostErrorHandlingTransformed = true;
    return rs.statementSemantic(sc);
}
