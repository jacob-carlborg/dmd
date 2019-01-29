module unit_test_runner;

import std.algorithm : filter, map, joiner, substitute;
import std.array : array, join;
import std.conv : to;
import std.file : dirEntries, exists, SpanMode, mkdirRecurse, write;
import std.format : format;
import std.getopt : getopt;
import std.path : buildNormalizedPath, buildPath, dirName, dirSeparator,
    stripExtension, setExtension;
import std.process : environment, spawnProcess, wait;
import std.range : empty;
import std.stdio;
import std.string : join, strip;

version (Posix)
    enum exeExtension = "";
else version (Windows)
    enum exeExtension = ".exe";

version(Windows)
    enum os ="windows";
else version(OSX)
    enum os ="osx";
else version(linux)
    enum os ="linux";
else version(FreeBSD)
    enum os ="freebsd";
else version(OpenBSD)
    enum os ="openbsd";
else version(NetBSD)
    enum os ="netbsd";
else version(DragonFlyBSD)
    enum os ="dragonflybsd";
else version(Solaris)
    enum os ="solaris";
else version(SunOS)
    enum os ="solaris";
else
    static assert(0, "Unrecognized or unsupported OS.");

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
        import core.runtime : Runtime, UnitTestResult;

        // modules to unit test starts here:
        %s

        enum modules = [
            %s
        ];

        enum filter = %s;

        version(unittest) shared static this()
        {
            Runtime.extendedModuleUnitTester = &unitTestRunner;
        }

        UnitTestResult unitTestRunner()
        {
            import std.algorithm : canFind, map;
            import std.format : format;
            import std.range : empty, front, enumerate;
            import std.stdio : writeln, writefln, stderr, stdout;
            import std.string : join;
            import std.conv : text;

            struct Test
            {
                Throwable throwable;
                string name;

                string toString()
                {
                    return format!"%%s\n%%s"(name, throwable);
                }

                string fileInfo()
                {
                    with (throwable)
                        return format!"%%s:%%s"(file, line);
                }
            }

            Test[] failedTests;
            size_t testCount;

            void printReport()
            {
                if (!failedTests.empty)
                {
                    alias formatTest = t =>
                        format!"%%s) %%s"(t.index + 1, t.value.toString);

                    const failedTestsMessage = failedTests
                        .enumerate
                        .map!(formatTest)
                        .join("\n");

                    stderr.writefln!"Failures:\n\n%%s\n"(failedTestsMessage);
                }

                auto output = failedTests.empty ? stdout : stderr;
                output.writefln!"%%s tests, %%s failures"(testCount, failedTests.length);

                if (failedTests.empty)
                    return;

                stderr.writefln!"\nFailed tests:\n%%s"(
                    failedTests.map!(t => t.fileInfo).join("\n"));
            }

            static foreach (m ; modules)
            {
                foreach (unitTest ; __traits(getUnitTests, mixin(m)))
                {
                    enum attributes = [__traits(getAttributes, unitTest)];

                    testCount++;
                    Test test;

                    try
                    {
                        static if (!attributes.empty)
                            test.name = attributes.front;

                        static if (filter.length == 0)
                            unitTest();

                        else if (attributes.front.canFind(filter))
                            unitTest();
                    }

                    catch (Throwable t)
                    {
                        test.throwable = t;
                        failedTests ~= test;
                    }
                }
            }

            printReport();

            UnitTestResult result = { runMain: false };
            return result;
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

/**
Writes a cmdfile with all the compiler flags to the given `path`.

Params:
    path = the path where to write the cmdfile file
    runnerPath = the path of the unit test runner file outputted by `writeRunnerFile`
    outputPath = the path where to place the compiled binary
    testFiles = the test files to compile
*/
void writeCmdfile(string path, string runnerPath, string outputPath,
    const string[] testFiles)
{
    enum staticFlags = [
        "-version=NoBackend",
        "-version=GC",
        "-version=NoMain",
        "-version=MARS",
        "-unittest",
        "-J" ~ projectRoot.buildPath("generated", "dub"),
        "-J" ~ projectRoot.buildPath("res"),
        "-I" ~ projectRoot.buildPath("src"),
        "-I" ~ scriptDir.buildPath("unit"),
        "-i",
        "-g",
        "-main"
    ];

    const flags = staticFlags ~ testFiles ~ runnerPath ~ ("-of" ~ outputPath);
    write(path, flags.join("\n"));
}

string dmdPath()
{
    const os = environment.get("OS", .os);
    const build = environment.get("BUILD", "release");
    const dmdFilename = "dmd".setExtension(exeExtension);

    const prefix = projectRoot.buildNormalizedPath("generated", os, build);
    const dmdModel = prefix.buildPath("64", dmdFilename).exists ? "64" : "32";
    const model = environment.get("MODEL", dmdModel);

    return prefix.buildPath(model, dmdFilename);
}

/**
Returns `true` if any of the given files don't exist.

Also prints an error message.
*/
bool missingTestFiles(const string[] givenFiles)
{
    const nonExistingTestFiles = givenFiles
        .map!(file => testPath(file))
        .filter!(file => !file.exists)
        .join("\n");

    if (!nonExistingTestFiles.empty)
    {
        stderr.writefln("The following test files don't exist:\n\n%s",
            nonExistingTestFiles);

        return true;
    }

    return false;
}


int main(string[] args)
{
    args ~= ["unit/deinitialization.d"];

    string unitTestFilter;
    getopt(args, "filter | f", &unitTestFilter);

    const givenFiles = args[1 .. $];

    if (missingTestFiles(givenFiles))
        return 1;

    enum runnerPath = resultsDir.buildPath("runner.d");
    const testFiles = givenFiles.testFiles;

    mkdirRecurse(resultsDir);
    testFiles
        .moduleNames
        .writeRunnerFile(runnerPath, unitTestFilter);

    enum cmdfilePath = resultsDir.buildPath("cmdfile");
    enum outputPath = resultsDir.buildPath("runner").setExtension(exeExtension);
    writeCmdfile(cmdfilePath, runnerPath, outputPath, testFiles);

    spawnProcess([dmdPath, "@" ~ cmdfilePath]).wait();
    spawnProcess(outputPath).wait();

    return 0;
}
