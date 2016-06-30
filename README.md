Slogan is a general purpose programming language with facilities for concurrent and parallel computing.

###Building and running Slogan

    $ sudo ./install

This will build and install slogan with `/etc/slogan` as SLOGAN_ROOT where all runtime
dependencies and packages are installed. SLOGAN_ROOT can be customized through a
command-line argument:

    $ sudo ./install /users/me/slogan
 
If you want to hack on Slogan itself, you may want to follow a more traditional approach:
    
    $ ./configure
    $ make
    $ make test
    $ sudo make install

If you follow this method, the current folder is treated as SLOGAN_ROOT. 

After the installation is complete, you can start the REPL by typing:
    
    $ slogan

(If you skipped system-wide installation, run `./src/slogan` instead.)


Visit http://schemer.in/slogan/ for tutorials and detailed documentation on the language.

You can help improve Slogan by reporting/fixing bugs and commenting on the enhancement proposals - https://github.com/vijaymathew/slogan/issues

Copyright (c) 2013, 2014, 2015, 2016 by Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>