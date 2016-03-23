if [ -s "$HOME/.dvm/scripts/dvm" ] ; then
    . "$HOME/.dvm/scripts/dvm" ;
    dvm use 2.071.1
fi

make -f posix.mak -j 16 DEBUG=1
