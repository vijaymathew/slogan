Slogan is a general purpose, high-level, expression oriented programming language.

This project strives to achieve the following goals:

    1. Design a language that is easy to learn. 

       Slogan is designed around a couple of simple concepts.
       Its syntax is familiar to the vast majority of programmers.

    2. Design a language that scales well. 

       Once the basics are mastered, no new syntax or concept needs to be 
       learned to program in the large.

    3. Build a language runtime that has distributed computing built-into its core.

This implementation, built on top of the Gambit Scheme platform, is the specification of the language.
It also acts as a playground for language design ideas. 
It has an interpreter and a compiler that generates platform specific executable binaries.

###Building and running Slogan

To build Slogan follow these steps:
 
  1. ./configure
  2. make
  3. cd src; ./slogan

The last step will land you in the Slogan REPL.

Slogan is still in the very early stages of design and your feedback is appreciated.
Please send your comments, suggestions and bug reports to vijay.the.lisper@gmail.com.