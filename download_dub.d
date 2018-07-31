// Downloads Dub. For use in the auto-tester CI environment.
module download_dub;

string archivePlatform(string model)
{
    version (FreeBSD)
        return platform ~ '-' ~ model;

    else
        return platform;
}

string binDir(string model)
{
    version (OSX)
        return "bin";
    else version (Windows)
        return "bin";
    else
        return "bin" ~ model;
}

version (FreeBSD)
    enum platform = "freebsd";

else version (linux)
    enum platform = "linux";

else version (OSX)
    enum platform = "osx";

else version (Windows)
    enum platform = "windows";

version (Posix)
{
    enum archiveExtension = "tar.xz";
    enum exeExtension = "";
}
else version (Windows)
{
    enum archiveExtension = "zip";
    enum exeExtension = ".exe";
}

string resolveLatest()
{
    import std.net.curl : get;
    import std.exception : assumeUnique;

    return get("http://downloads.dlang.org/releases/LATEST").assumeUnique;
}

string dmdUrl(string model)
{
    import std.format : format;

    enum fmt = "http://downloads.dlang.org/releases/2.x/%s/dmd.%s.%s.%s";
    const version_ = resolveLatest;
    return format!fmt(version_, version_, archivePlatform(model), archiveExtension);
}

void unpack(string dmdArchivePath)
{
    import std.process : spawnProcess, wait;

    version (Posix)
        const arguments = ["tar", "xf", dmdArchivePath];
    else version (Windows)
        const arguments = ["unzip", dmdArchivePath];

    spawnProcess(arguments).wait();
}

void extractDub(string path, string dest, string model)
{
    import std.path : buildPath;
    import std.file : rename, exists;
    import std.process : spawnProcess, wait;

    enum filename = "dub" ~ exeExtension;
    const binPath = buildPath(path, platform, binDir(model));
    const dubPath = buildPath(binPath, filename);

    import std.stdio;

    writefln("dubPath=%s dest=%s", dubPath, buildPath(dest, filename));

    import std.file : dirEntries, SpanMode;

    writeln("cwd:");
    foreach (e ; dirEntries(".", SpanMode.shallow))
        writeln(e);

    writefln("bin (%s) folder:", binPath);
    foreach (e ; dirEntries(buildPath(path, platform, binDir(model)), SpanMode.shallow))
        writeln(e);

    writefln("exists: %s %s", dubPath, dubPath.exists);
    writefln("exists: %s %s", dest, dest.exists);

    // needed for Windows
    spawnProcess(["chmod", "+x", dubPath]).wait();
    rename(dubPath, buildPath(dest, filename));
}

void cleanup(string dmdArchivePath, string dmdPath)
{
    import std.file : rmdirRecurse;
    import std.file : remove;

    rmdirRecurse(dmdPath);
    remove(dmdArchivePath);
}

void main(string[] args)
{
    import std.file : getcwd;
    import std.net.curl : download;
    import std.process : environment;

    enum dmdArchivePath = "dmd." ~ archiveExtension;
    enum dmdPath = "dmd2";

    if (args.length < 2)
        throw new Exception("Missing MODEL");

    const model = args[1];

    download(dmdUrl(model), dmdArchivePath);
    unpack(dmdArchivePath);
    extractDub(dmdPath, getcwd, model);
    cleanup(dmdArchivePath, dmdPath);
}
