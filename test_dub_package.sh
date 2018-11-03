#!/bin/bash

set -euxo pipefail

test_dub_package() {
    local build_path=$1

    # GDC's standard library is too old for some example scripts
    if [[ "${DMD:-dmd}" =~ "gdmd" ]] ; then
        echo "Skipping DUB examples on GDC."
    else
        local abs_build_path="$PWD/$build_path"
        pushd test/dub_package
        for file in *.d ; do
            # build with host compiler
            dub --single "$file"
            # build with built compiler (~master)
            DFLAGS="-de" dub --single --compiler="${abs_build_path}/dmd" "$file"
        done
        popd
        # Test rdmd build
        "${build_path}/dmd" -version=NoBackend -version=GC -version=NoMain -Jgenerated/dub -Jres -Isrc -i -run test/dub_package/frontend.d
    fi
}

test_dub_package $1
