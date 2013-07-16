// Compiler implementation of the D programming language
// Copyright (c) 2013 by Digital Mars
// All Rights Reserved
// written by Jacob Carlborg
// http://www.digitalmars.com
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.

#ifndef DMD_DSTEP_H
#define DMD_DSTEP_H

#include "clang/clang.h"

#ifdef __cplusplus
extern "C" {
#endif

bool rt_init (void*);
bool rt_term (void*);

enum Language
{
    Language_C,
	Language_ObjC
};

struct TranslationArgs
{
	const char* file;
	const char* outputFile;
	const char** args;
	size_t argsLength;
	Language language;
};

struct Translator
{
	CXIndex index;
	CXTranslationUnit translationUnit;
};

int dstep_translate (TranslationArgs args, Translator* translator);
void dstep_disposeTranslator (Translator translator);
bool dstep_shouldDisposeTranslator (Translator translator);

#ifdef __cplusplus
}
#endif

#endif /* DMD_DSTEP_H */