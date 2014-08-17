----
title: Installing Liquid Haskell
----

I'm looking to try out [**Liquid Haskell**] [liquid-haskell] in preparation for a blog post so figured I would document the installation experience.

(So I'll save the details and motivation for looking at **Liquid Haskell** until then)

Its more to document the inconvenience and hoops I had to jump through. Part of what I would like to comment on is the hassle/ease of installing it.

## Prequisites

So **Liquid Haskell** requires the following:

-   A recent **Ocaml** compiler...
-   [**Z3**] [z3]

### Installing the latest version of Git

I was running the version of **Git** from the aptitude `git` package (version `1.9.1`) and was getting this error:

```
% git clone https://git01.codeplex.com/z3
Cloning into 'z3'...
remote: Counting objects: 23690, done.
remote: Compressing objects: 100% (5648/5648), done.
remote: Total 23690 (delta 18425), reused 23042 (delta 17944)
Receiving objects: 100% (23690/23690), 9.65 MiB | 184.00 KiB/s, done.
error: RPC failed; result=56, HTTP code = 200
Resolving deltas: 100% (18425/18425), done.
```

Apparently the problem is due to the PPA distributed binaries being linked to `libcurl3-openssl`, so the solution is to build them from source and link them to `libcurl4-openssl`

The following builds and installs latest version of `git` (version `2.0.3` at the time of this writing) from source.

```
% sudo apt-get install gettext zlib1g-dev asciidoc libcurl4-openssl-dev
% git clone git@github.com:git/git.git
% cd git
% git checkout v2.0.3
% make configure
% ./configure --prefix=/usr
% make all doc
% sudo make install install-doc install-html
```

### Installing Z3

Instructions for installing **Z3** for Ubuntu are [**here**] [installing-z3] (little hard to find).

But just in case whatever, they are again listed here (I didn't want to install it to `/usr/bin`, but `~/bin` instead):

Its also important to install version `4.3.2` and above.

```
% git clone https://git01.codeplex.com/z3
% cd z3
% autoconf
% ./configure --prefix=~/bin
% python scripts/mk_make.py
% cd build
% make -j8
% sudo make install
```

I had previously tried `4.3.1` as it was the "stable" release, however when I eventually tried verifying a module, I got the following error:

```
Error setting 'MODEL.PARTIAL', reason: unknown option.
ERROR: invalid INI file
Fatal error: exception Failure("bracket hits exn: Failure("bracket hits exn: End_of_file \n") 
")
```

When I replaced it with the `4.3.2` version of `z3`, it worked.

### Building the OCaml Compiler

Instructions are [**here**] [installing-ocaml].

Again just in case and to compare:

```
% git clone git@github.com:ocaml/ocaml.git
% cd ocaml
% git checkout 4.01.0
% ./configure
% make world
% make bootstrap
% make opt
% make opt.opt
% umask 022
% make install
% make clean
```

## Installing GHC version 4.8.3

Liquid Haskell needs **GHC**, version `4.8.3`, instructions to build it from source are [**here**] [installing-ghc]

## Installing Liquid Haskell

```
% mkdir -p ~/haskell-tools/liquid-haskell
% cd ~/haskell-tools/liquid-haskell
% git clone git@github.com:ucsd-progsys/liquid-fixpoint.git
% git clone git@github.com:ucsd-progsys/liquidhaskell.git
% cabal sandbox init
% cabal sandbox add-source liquid-fixpoint
% cabal sandbox add-source liquidhaskell
% cabal install liquid-fixpoint
% cabal install liquidhaskell
% mkdir -p ~/haskell-tools/bin
% cd ~/haskell-tools/bin
% ln -sf ~/haskell-tools/liquid-haskell/.cabal-sandbox/bin/liquid .
% ln -sf ~/haskell-tools/liquid-haskell/.cabal-sandbox/bin/fixpoint .
```

[liquid-haskell]: http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/about/ "About Liquid Haskell"
[z3]: http://z3.codeplex.com/ "Z3"
[installing-z3]: http://z3.codeplex.com/SourceControl/latest#README "Z3 Installation Instructions"
[installing-ocaml]: https://github.com/ocaml/ocaml/blob/trunk/INSTALL "OCaml Installation Instructions"
[installing-ghc]: https://ghc.haskell.org/trac/ghc/wiki/Building/QuickStart "GHC Installation Instructions"
