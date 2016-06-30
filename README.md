Slogan is a general purpose programming language with facilities for concurrent and parallel computing.

###Building and running Slogan

Method 1: 

    $ sudo ./install

This will copy the slogan executable program to `/usr/local/bin`.
`/etc/slogan` is the default SLOGAN_ROOT where all runtime dependencies and packages are installed.
SLOGAN_ROOT can be customized through the command-line:

    $ sudo ./install /users/me/slogan
 
Method 2:
    
    $ ./configure
    $ make
    $ make test
    $ sudo make install

If you follow this method, the current folder is treated as SLOGAN_ROOT. 

Now you are ready to launch the Slogan REPL:
    
    $ slogan

(If you skipped system-wide installation, run `./src/slogan` instead.)


Visit http://schemer.in/slogan/ for tutorials and detailed documentation on the language.

You can help improve Slogan by reporting/fixing bugs and commenting on the enhancement proposals - https://github.com/vijaymathew/slogan/issues

Copyright (c) 2013, 2014, 2015, 2016 by Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>