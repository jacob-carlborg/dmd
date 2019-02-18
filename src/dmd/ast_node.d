/**
 * Compiler implementation of the
 * $(LINK2 http://www.dlang.org, D programming language).
 *
 * Copyright:   Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/serializer.d, _serializer.d)
 * Documentation:  https://dlang.org/phobos/dmd_serializer.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/serializer.d
 */
module dmd.ast_node;

import dmd.root.rootobject : RootObject;
import dmd.visitor : Visitor;

extern (C++) class ASTNode : RootObject
{
    import dmd.root.serializer : Serialzier;

    abstract void accept(Visitor v);

    void serialize(ref Serialzier serializer)
    {
        extern (C++) final class SerializationVisitor : Visitor
        {
            import dmd.attrib;
            import dmd.aliasthis;
            import dmd.aggregate;
            import dmd.complex;
            import dmd.cond;
            import dmd.ctfeexpr;
            import dmd.dclass;
            import dmd.declaration;
            import dmd.denum;
            import dmd.dimport;
            import dmd.declaration;
            import dmd.dstruct;
            import dmd.dsymbol;
            import dmd.dtemplate;
            import dmd.dversion;
            import dmd.expression;
            import dmd.func;
            import dmd.denum;
            import dmd.dimport;
            import dmd.dmodule;
            import dmd.mtype;
            import dmd.typinf;
            import dmd.identifier;
            import dmd.init;
            import dmd.doc;
            import dmd.root.rootobject;
            import dmd.statement;
            import dmd.staticassert;
            import dmd.nspace;

            alias visit = Visitor.visit;

            static string generateVisitors()
            {
                string code;

                foreach (overload; __traits(getOverloads, Visitor, "visit"))
                {
                    static if (is(typeof(overload) Parameters == function))
                    {
                        code ~= ` override void visit (` ~ Parameters[0].stringof ~ ` v)
                            {
                                serializer.serialize(v);
                            }

                        `;
                    }
                }

                return code;
            }

            mixin(generateVisitors);
        }

        scope visitor = new SerializationVisitor();
        accept(visitor);
    }
}
