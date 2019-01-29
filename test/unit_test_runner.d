module unit_test_runner;

import std.algorithm : filter, map, joiner, substitute;
import std.array : array, join;
import std.conv : to;
import std.file : dirEntries, exists, SpanMode, mkdirRecurse, write;
import std.format : format;
import std.getopt : getopt;
import std.path : buildNormalizedPath, dirName, stripExtension, dirSeparator;
import std.process : spawnProcess, wait;
import std.range : empty;
import std.stdio;
import std.string : join, strip;

enum scriptDir = __FILE_FULL_PATH__.dirName.buildNormalizedPath;
enum projectRoot = scriptDir.buildNormalizedPath("..");
enum unitTestDir = scriptDir.buildNormalizedPath("unit");
enum resultsDir = testPath("test_results");

alias testPath = path => buildNormalizedPath(scriptDir, path);

string[] testFiles(const string[] args)
{
    if (args.length)
        return args.map!(testPath).array;

    return unitTestDir
        .dirEntries("*.d", SpanMode.depth)
        .map!(e => e.name)
        .array;
}

auto moduleNames(const string[] testFiles)
{
    return testFiles
        .map!(e => e[unitTestDir.length + 1 .. $])
        .map!stripExtension
        .array
        .map!(e => e.substitute(dirSeparator, "."));
}

void writeRunnerFile(Range)(Range moduleNames, string path, string filter)
{
    enum codeTemplate = q{
        %s

        enum modules = [
            %s
        ];

        enum filter = %s;

        void main()
        {
            static foreach (m ; modules)
            {
                foreach (unitTest ; __traits(getUnitTests, mixin(m)))
                {
                    enum attributes = [__traits(getAttributes, unitTest)];

                    static if (filter.length == 0)
                        unitTest();

                    else if (attributes.canFind!(e => e.canFind(filter)))
                        unitTest();
                }
            }
        }
    };

    const imports = moduleNames
        .map!(e => format!"import %s;"(e))
        .joiner("\n")
        .to!string;

    const modules = moduleNames
        .map!(e => format!`"%s"`(e))
        .joiner(",\n")
        .to!string;

    const content = format!codeTemplate(imports, modules, format!`"%s"`(filter));
    write(path, content);
}

void writeCmdfile(string path, string runnerPath, const string[] testFiles)
{
    enum staticFlags = [
        "-version=NoBackend",
        "-version=GC",
        "-version=NoMain",
        "-version=MARS",
        "-unittest",
        "-J" ~ projectRoot.buildNormalizedPath("generated", "dub"),
        "-J" ~ projectRoot.buildNormalizedPath("res"),
        "-I" ~ projectRoot.buildNormalizedPath("src"),
        "-I" ~ scriptDir.buildNormalizedPath("unit"),
        "-i"
    ];

    const flags = staticFlags ~ testFiles;
    write(path, flags.join("\n"));
}

int main(string[] args)
{
    args ~= ["unit/deinitialization.d"];

    string unitTestFilter;
    getopt(args, "filter | f", &unitTestFilter);

    const givenFiles = args[1 .. $];

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

    const runnerPath = resultsDir.buildNormalizedPath("runner.d");
    const testFiles = givenFiles.testFiles;

    mkdirRecurse(resultsDir);
    testFiles
        .moduleNames
        .writeRunnerFile(runnerPath, unitTestFilter);

    const cmdfilePath = resultsDir.buildNormalizedPath("cmdfile");
    writeCmdfile(cmdfilePath, runnerPath, testFiles);


    const dmdPath = projectRoot.buildNormalizedPath("generated/osx/release/64/dmd");
    writeln([dmdPath, "@" ~ cmdfilePath, "-run", runnerPath].join(" "));
    // spawnProcess([dmdPath, "@" ~ cmdfilePath]).wait();

    return 0;
}
