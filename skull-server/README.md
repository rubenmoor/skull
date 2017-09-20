# prerequisites

## postgresql

Switch to postgres user

    sudo -u postgres -i

Create postgres super user

    postgres# createuser -s postgres

Start postgres shell

    postgres# psql

# create user

    psql# create user skull with password 'skull';

# create database

Create database named 'test' for the user 'skull'.

    psql# create database test owner skull;

# configure

    $ nix-shell -I ~ --command 'cabal configure'

# build

    $ cabal build

# run code generator

    # dist/build/psclient-generator/psclient-generator
    
# run server

    $ dist/build/skull-server-exe/skull-server-exe
