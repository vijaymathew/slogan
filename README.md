Slogan is a general purpose expression oriented programming language 
with facilities for concurrent computing.

This implementation comes with an interpreter and a compiler for 
generating platform specific executable binaries.

###Building and running Slogan

Follow these steps to build Slogan:

    $ ./configure-platform
    $ make
    $ cd src; ./slogan

The last step will land you in the Slogan REPL.
For the Slogan compiler to work properly, you have to point the environment variable 
`SLOGAN_ROOT' to this folder:

    $ export SLOGAN_ROOT=/full_path_to_the_slogan_folder

Slogan is still in the very early stages of design and your feedback is highly valued.
Please send your comments, suggestions and bug reports to vijay.the.lisper@gmail.com.
