module unit_test_runner;

import std.algorithm : map, substitute;
import std.array : array;
import std.file : dirEntries, SpanMode;
import std.path : buildNormalizedPath, dirName, stripExtension, dirSeparator;
import std.stdio;

enum scriptDir = __FILE_FULL_PATH__.dirName.buildNormalizedPath;
alias testPath = path => buildNormalizedPath(scriptDir, path);

string[] testFiles(const string[] args)
{
    if (args.length)
        return args.map!(testPath).array;

    return scriptDir
        .buildNormalizedPath("unit")
        .dirEntries("*.d", SpanMode.depth)
        .map!(e => e.name)
        .array;
}

auto moduleNames(const string[] testFiles)
{
    return testFiles
        .map!(e => e[scriptDir.length + 1 .. $])
        .map!stripExtension
        .array
        .map!(e => e.substitute("/", "."));
}

int main(string[] args)
{
    args ~= ["unit/deinitialization.d"];
    const givenFiles = args[1 .. $];

    import std.algorithm : filter, map;
    import std.array : join;
    import std.range : empty;
    import std.file : exists;

    const nonExistingTestFiles = givenFiles
        .map!(file => testPath(file))
        .filter!(file => !file.exists)
        .join("\n");

    if (!nonExistingTestFiles.empty)
    {
        stderr.writefln("The following test files don't exist:\n\n%s",
            nonExistingTestFiles);

        return 1;
    }

    auto a = givenFiles.testFiles.moduleNames();
    writeln(a);
    return 0;
}
