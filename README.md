Slogan is a general purpose functional programming language with facilities for concurrent and parallel computing.

#### Features

    - Dynamic and strong type system
    - Rich library: data structures, I/O, networking
    - Unicode support
    - Efficient concurrency: execute tens-of-thousands of independent tasks
    - Multi-core parallelism based on a simple message-passing model
    - Declarative programming: reactive variables, pattern matching
    - Polymorphism, multiple dispatch
    - Hygienic and unhygienic syntax extensions
    - REPL for fast program design and development
    - Compiles to optimized stand-alone binaries
    - Easy integration with C libraries
    - Modules and namespaces for program organization
    - Built-in package manager

#### Building and running Slogan

    $ sudo ./install

After the installation is complete, you can start the REPL by typing:

    $ slogan


You can also install Slogan using explicit configure & make commands:
    
    $ ./configure
    $ make
    $ make test
    $ sudo make install

This would be useful if you are hacking on the Slogan implementation itself.
If you skip system-wide installation, note that the `slogan` binary will be created
in the `src` folder and can be run as `./src/slogan`.


Please visit http://schemer.in/slogan/ for tutorials and detailed documentation on the language.

Slogan is under active development and can be considered beta quality.
Please report bugs/issues at https://github.com/vijaymathew/slogan/issues.

Copyright © 2013, 2014, 2015, 2016, 2017 by Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>