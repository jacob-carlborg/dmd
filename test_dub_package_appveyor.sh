set -uexo pipefail

export MODEL=${MODEL:-64}      # can be {32,64}
export DMD=${DMD:-dmd}         # can be {dmd,ldc,gdc}
export OS_NAME=${OS_NAME:-windows}

sh /c/projects/dmd/test_dub_package.sh "generated/$OS_NAME/release/$MODEL"
