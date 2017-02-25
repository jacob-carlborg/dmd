module ddmd.ast_printer;

import ddmd.asttypename;

public void printAst(T)(T node)
{
    import core.stdc.stdarg;
    import ddmd.root.outbuffer;

    static const(char)[] escape(const(char)[] str)
    {
        size_t j;

        static const(char)[] replace(const(char)[] s, size_t i, string replacement)
        {
            return s[0 .. i] ~ replacement ~ s[i + 1 .. $];
        }

        foreach (i, e ; str)
        {
            if (e == '\n')
                str = replace(str, i, "\\n");

            else if (e == '"')
                str = replace(str, i, "\\\"");
        }

        return str;
    }

    extern (C++) final scope class AstPrintVisitor : Visitor
    {
        OutBuffer* buffer;
        int level;
        enum indentation = 2;
        bool shouldIdent;

        alias visit = Visitor.visit;

        this(OutBuffer* buffer)
        {
            this.buffer = buffer;
        }

        override void visit(Identifier ident)
        {
            appendf("Identifier(%s)", ident.toChars);
        }

        override void visit(Statement s)
        {
            assert(0, "No implementation for the statement: " ~ astTypeName(s));
        }

        void visitBase(Statement)
        {
            node("Statement");
        }

        override void visit(CompoundStatement s)
        {
            node(s, {
                fields("statements", s.statements);
            });
        }

        void visitBase(CompoundStatement s)
        {
            visit(s);
        }

        override void visit(ExpStatement s)
        {
            node(s, {
                fields("exp", s.exp);
            });
        }

        override void visit(ForeachStatement s)
        {
            node(s, {
               fields(
                   "op", s.op,
                   "parameters", s.parameters,
                   "aggr", s.aggr,
                   "_body", s._body
                );
            });
        }

        override void visit(ReturnStatement s)
        {
            node(s, {
                fields("exp", s.exp);
            });
        }

        override void visit(Expression e)
        {
            assert(0, "No implementation for the expression: " ~ astTypeName(e));
        }

        void visitBase(Expression e)
        {
            node("Expression", {
                fields(
                    "type", e.type,
                    "op", e.op,
                );
            });
        }

        override void visit(ArrayLiteralExp e)
        {
            node(e, {
                fields("elements", e.elements);
            });
        }

        override void visit(AssignExp e)
        {
            node(e);
        }

        override void visit(AstMacroResultStatementExp e)
        {
            e.statement.accept(this);
        }

        override void visit(BinExp e)
        {
            node(e, {
                fields(
                    "e1", e.e1,
                    "e2", e.e2
                );
            });
        }

        void visitBase(BinExp e)
        {
            visit(e);
        }

        override void visit(CallExp e)
        {
            node(e, {
                fields(
                    "arguments", e.arguments,
                    "f", e.f
                );
            });
        }

        override void visit(ClassReferenceExp e)
        {
            node(e, {
                fields("value", e.value);
            });
        }

        override void visit(FuncExp e)
        {
            node(e, {
                fields("fd", e.fd);
            });
        }

        override void visit(IdentifierExp e)
        {
            node(e, {
                fields("ident", e.ident);
            });
        }

        override void visit(IntegerExp e)
        {
            node(e, {
                fields("value", e.toInteger);
            });
        }

        override void visit(NewExp e)
        {
            node(e, {
                fields(
                    "thisexp", e.thisexp,
                    "newargs", e.newargs,
                    "newtype", e.newtype,
                    "arguments", e.arguments,
                    "member", e.member
                );
            });
        }

        override void visit(NullExp e)
        {
            node(e);
        }

        override void visit(StringExp e)
        {
            node(e, {
                fields("string", `"` ~ escape(e.peekSlice) ~ `"`);
            });
        }

        override void visit(StructLiteralExp e)
        {
            node(e, {
                fields(
                    "sd", e.sd,
                    "elements", e.elements,
                    "type", e.type
                );
            });
        }

        override void visit(SymbolExp e)
        {
            node(e, {
                fields("var", e.var);
            });
        }

        void visitBase(SymbolExp e)
        {
            visit(e);
        }

        override void visit(ThisExp e)
        {
            node(e, {
                fields("var", e.var);
            });
        }

        override void visit(UnaExp e)
        {
            node(e, {
                fields(
                    "expression", e.e1
                );
            });
        }

        void visitBase(UnaExp e)
        {
            visit(e);
        }

        override void visit(VarExp e)
        {
            node(e);
        }

        override void visit(Dsymbol s)
        {
            assert(0, "No implementation for the symbol: " ~ astTypeName(s));
        }

        void visitBase(Dsymbol s)
        {
            node("Dsymbol", {
                fields("ident", s.ident);
            });
        }

        override void visit(ScopeDsymbol s)
        {
            node(s, {
                // fields("members", s.members);
            });
        }

        void visitBase(ScopeDsymbol s)
        {
            visit(s);
        }

        override void visit(Declaration d)
        {
            node(d, {
                fields("type", d.type);
            });
        }

        void visitBase(Declaration d)
        {
            visit(d);
        }

        override void visit(AggregateDeclaration ad)
        {
            node(ad, {
                //fields("type", ad.type);
            });
        }

        void visitBase(AggregateDeclaration ad)
        {
            visit(ad);
        }

        override void visit(AttribDeclaration ad)
        {
            node(ad);
        }

        void visitBase(AttribDeclaration ad)
        {
            visit(ad);
        }

        override void visit(ClassDeclaration cd)
        {
            node(cd);
        }

        override void visit(CtorDeclaration cd)
        {
            node(cd);
        }

        override void visit(EnumDeclaration ed)
        {
            node(ed, {
                // fields("memtype", ed.memtype);
            });
        }

        override void visit(FuncDeclaration fd)
        {
            node(fd, {
                fields(
                    "fbody", fd.fbody,
                    "fes", fd.fes,
                );
            });
        }

        void visitBase(FuncDeclaration fd)
        {
            visit(fd);
        }

        override void visit(FuncLiteralDeclaration fd)
        {
            node(fd, {
                fields("tok", fd.tok);
            });
        }

        override void visit(ProtDeclaration pd)
        {
            node(pd);
        }

        override void visit(Type t)
        {
            assert(0, "No implementation for the type: "  ~ astTypeName(t));
        }

        void visitBase(Type t)
        {
            // auto name = enumValueToString(cast(ENUMTY) t.ty);
            node("Type", {
                fields(
                    "ty", t.ty,
                    "deco", t.deco
                );
            });
            // appendf("Type(ty: %.*s)", name.length, name.ptr);
        }

        override void visit(TypeArray t)
        {
            node(t);
        }

        void visitBase(TypeArray t)
        {
            visit(t);
        }

        override void visit(TypeDArray t)
        {
            node(t);
        }

        override void visit(TypeBasic t)
        {
            node(t, {
                fields("dstring", t.dstring);
            });
        }

        override void visit(TypeClass t)
        {
            node(t, {
                fields("sym", t.sym);
            });
        }

        override void visit(TypeEnum t)
        {
            node(t, {
                fields("sym", t.sym);
            });
        }

        override void visit(TypeFunction t)
        {
            node(t, {
                fields(
                    "parameters", t.parameters,
                    "varargs", t.varargs,
                    "linkage", t.linkage,
                );
            });
        }

        override void visit(TypeIdentifier t)
        {
            node(t, {
                fields("ident", t.ident);
            });
        }

        override void visit(TypeNext t)
        {
            node(t, {
                fields("next", t.next);
            });
        }

        void visitBase(TypeNext t)
        {
            visit(t);
        }

        override void visit(TypePointer t)
        {
            node(t);
        }

        override void visit(Parameter p)
        {
            node("Parameter", {
                fields(
                    "storageClass", p.storageClass,
                    "type", p.type,
                    "ident", p.ident,
                    "defaultArg", p.defaultArg
                );
            });
        }

        override void visit(TypeQualified t)
        {
            node(t);
        }

        void visitBase(TypeQualified t)
        {
            visit(t);
        }

        extern (D):

        void node(T)(T astNode, void delegate() block = null)
        {
            node(T.stringof, {
                visitBase(cast(BaseType!T) astNode);

                if (block)
                {
                    append(',');
                    newline();
                    block();
                }
            });
        }

        void node(string name, void delegate() block = null)
        {
            append(name);

            if (block)
                bracket(block);
            else
                append(" {}");
        }

        void fields(Fields...)(Fields fields) if (Fields.length % 2 == 0)
        {
            foreach (i, f ; fields)
            {
                static if (is(typeof(f) == string))
                {
                    if (i != 0)
                    {
                        append(',');
                        newline();
                    }

                    static if (is(Fields[i + 1] == enum))
                        fieldWithEnumValue(f, fields[i + 1]);
                    else
                        field(f, fields[i + 1]);
                }
                else
                    continue;
            }
        }

        void field(Node)(string name, Node node)
        {
            append(name);
            append(": ");

            if (node)
                node.accept(this);
            else
            {
                enum nodeName = Node.stringof;
                appendf("%.*s(null)", nodeName.length, nodeName.ptr);
            }
        }

        void field(T : int)(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%d", value);
        }

        void field(T : ulong)(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%d", value);
        }

        void field(T : const(char)*)(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%s", value);
        }

        void field(T : const(char)[])(string name, T value)
        {
            append(name);
            append(": ");
            appendf("%.*s", value.length, value.ptr);
        }

        void field(T : string)(string name, T value)
        {
            field(name, cast(const(char)[]) value);
        }

        void fieldWithEnumValue(T)(string name, T value) if (is(T == enum))
        {
            field(name, enumValueToString(value));
        }

        void field(T : E*, E)(string name, T array)
        {
            append(name);
            append(":");

            if (!array)
                append(" null");

            else if (array.dim == 0)
                append(" []");

            else
            {
                bracket('[', ']', {
                    foreach (i, e ; *array)
                    {
                        if (i != 0)
                        {
                            append(',');
                            newline();
                        }

                        e.accept(this);
                    }
                });
            }
        }

        void bracket(void delegate() block)
        {
            bracket('{', '}', block);
        }

        void bracket(char s, char e, void delegate() block)
        {
            newline();
            append(s);
            newline();
            indent(block);
            newline();
            append(e);
        }

        void indent(void delegate() block)
        {
            level++;

            scope (exit)
                level--;

            block();
        }

        void indent()
        {
            if (shouldIdent)
            {
                foreach (_ ; 0 .. level * indentation)
                    buffer.writeByte(' ');
            }
        }

        void newline()
        {
            buffer.writenl();
            shouldIdent = true;
        }

        void append(string str)
        {
            indent();
            buffer.writestring(str);
            shouldIdent = false;
        }

        void append(char c)
        {
            indent();
            buffer.writeByte(c);
            shouldIdent = false;
        }

        extern (C++) void appendf(const(char)* format, ...) /*nothrow*/
        {
            indent();
            va_list ap;
            va_start(ap, format);
            buffer.vprintf(format, ap);
            va_end(ap);
            shouldIdent = false;
        }
    }

    OutBuffer buffer;

    scope visitor = new AstPrintVisitor(&buffer);
    node.accept(visitor);

    printf("%s\n", buffer.extractString);
}
