# prerequisites

# nix

configure

    $ nix-shell -I ~ --command 'cabal configure'

and build

    $ cabal build

run code generator

    # dist/build/psclient-generator/psclient-generator
    
run server

    $ dist/build/skull-server-exe/skull-server-exe

or

# stack

    $ stack build
    $ stack exec psclient-generator
    $ stack exec skull-server-exe
