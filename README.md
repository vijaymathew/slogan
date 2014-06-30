Slogan is a general purpose expression oriented programming language 
with facilities for concurrent computing.

This implementation comes with an interpreter and a compiler for 
generating platform specific executable binaries.

###Building and running Slogan

Follow these steps to build Slogan:

    $ ./configure
    $ make
    $ sudo make install
    $ slogan

The last step will land you in the Slogan REPL. (If you don't want to install Slogan,
you can just run 'make' as the current user and launch the REPL by executing './src/slogan').
For the Slogan compiler to work properly, you have to point the environment variable 
'SLOGAN_ROOT' to this directory:

    $ export SLOGAN_ROOT=/home/me/dir_where_i_checked_out_slogan

Please send your comments, suggestions and bug reports to vijay.the.lisper@gmail.com.
