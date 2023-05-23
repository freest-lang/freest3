#!/bin/bash

# Create folder on tmp and copy files

version=`cat ../FreeST/package.yaml | grep -e "version:" |  sed -E 's/version: *//g'`

# 

path=freest-$version/
freest_path=$path/FreeST
repl_path=$path/REPL/
examples_path=$path/examples/
stdlib_path=$freest_path/StandardLib/
pkg_path=$path/packages
freest_mode_path=$pkg_path/freest-mode/

# Create release dirs
mkdir $path
mkdir $freest_path
mkdir $examples_path
mkdir $repl_path
mkdir $stdlib_path
mkdir $pkg_path
mkdir $freest_mode_path


# Copy FreeST, REPL, StdLib, LICENSE and CHANGELOG 
cp -R ../FreeST/src $freest_path
[ -e $freest_path/src/Parse/Parser.hs ] && rm $freest_path/src/Parse/Parser.hs
[ -e $freest_path/src/Parse/Lexer.hs ] && rm $freest_path/src/Parse/Lexer.hs
rm -rf $freest_path/src/.stack-work/

cp -R ../REPL/src $repl_path
cp ../REPL/package.yaml $repl_path
cp -R ../FreeST/StandardLib/ $stdlib_path
cp ../LICENSE ../CHANGELOG $path

# Copy packages
# Suppose that you have the project freest-mode cloned
# (at the same level of this one)
cp -r ../../freest-mode/emacs/freest-mode/freest-mode.el  ../../freest-mode/emacs/freest-mode/freest-font-lock.el ../../freest-mode/emacs/freest-mode/ob-freest.el $freest_mode_path
cp -r ../../freest-mode/atom/ $pkg_path


# Copy test examples into release
cp ../FreeST/test/Programs/ValidTests/SessionTypes/anbn/anbn.fst  $examples_path/AnBn.fst
cp ../FreeST/test/Programs/ValidTests/SessionTypes/arithExprServer/arithExprServer.fst  $examples_path/ArithExprServer.fst
cp ../FreeST/test/Programs/ValidTests/SessionTypes/crisscross/crisscross.fst  $examples_path/Crisscross.fst
cp ../FreeST/test/Programs/ValidTests/Functional/fixZcombinator/fixZcombinator.fst $examples_path/FixZcombinator.fst
cp ../FreeST/test/Programs/ValidTests/SessionTypes/lazyTreeTraversal/lazyTreeTraversal.fst $examples_path/LazyTreeTraversal.fst
cp ../FreeST/test/Programs/ValidTests/Applications/ordering/ordering.fst $examples_path/Ordering.fst
cp ../FreeST/test/Programs/ValidTests/SessionTypes/DyckWords/DyckWords.fst $examples_path/
cp ../FreeST/test/Programs/ValidTests/SessionTypes/Unnormed/Unnormed.fst  $examples_path/
cp ../FreeST/test/Programs/ValidTests/SessionTypes/TreeTransform/TreeTransform.fst  $examples_path/

resolverVersion=`cat ../stack.yaml | grep -e "^resolver:" | sed -E 's/resolver: *//g'`

# Create stack.yaml file

echo "# FreeST stack file
# Authors: Bernardo Almeida, Vasco T. Vasconcelos, Andreia Mordido

# Resolver to choose a 'specific' stackage snaps\hot or a compiler version.
resolver: $resolverVersion

# packages to build
packages:
- FreeST
- REPL" > $path/stack.yaml

# Create package.yaml file

package=$freest_path/package.yaml

name=`cat ../FreeST/package.yaml | grep -e "name:"`
license=`cat ../FreeST/package.yaml | grep -e "license:"`
author=`cat ../FreeST/package.yaml | grep -e "author:"`
copyright=`cat ../FreeST/package.yaml | grep -e "copyright:"`
datadir=`cat ../FreeST/package.yaml | grep -e "data-dir:"`
datafiles=`sed -n -e '/data-files:/,/^$/ p' ../FreeST/package.yaml`
# datafiles=`cat ../FreeST/package.yaml | grep -e "data-files:"`
library=`sed -n -e '/library:/,/^$/ p' ../FreeST/package.yaml`


echo $name >> $package
echo "version: "$version >> $package
echo $license >> $package
echo $author >> $package
echo $copyright >> $package
echo "" >> $package
echo $datadir >> $package
echo "$datafiles" >> $package
echo "" >> $package
echo "$(awk '/^dependencies:/,/^$/' ../FreeST/package.yaml)" >> $package
echo "" >> $package
echo "$(awk '/^executables:/,/^$/' ../FreeST/package.yaml)" >> $package
echo "" >> $package
echo "$library" >> $package
echo "" >> $package


# Create README.md

echo "\`\`\`
  ______              _____ _______ 
 |  ____|            / ____|__   __|
 | |__ _ __ ___  ___| (___    | |
 |  __| '__/ _ \/ _ \\___ \   | |
 | |  | | |  __/  __/____) |  | |
 |_|  |_|  \___|\___|_____/   |_|
\`\`\`

# Install stack
- For Un*x-like operating systems:
\`\`\`
    $ curl -sSL https://get.haskellstack.org/ | sh
\`\`\`

- For windows download the installer available at: 
  
  [Get stack for windows](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

For more information about stack please visit [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

# Install FreeST

- Extract the zip FreeST-$version.zip

\`\`\`
$ cd freest-$version/
\`\`\`

- Install FreeST
\`\`\`
$ stack install FreeST
\`\`\`

- Install REPL (freesti)
\`\`\`
$ stack install REPL
\`\`\`

- Install both
\`\`\`
$ stack install
\`\`\`

If you get the following warning:
\`\`\`
Warning: Installation path /home/.../.local/bin
         not found on the PATH environment variable.
\`\`\`

add this path to your PATH environment variable: 
\`\`\`
export PATH=/home/.../.local/bin/:\$PATH
\`\`\`

this will only work for the current terminal session. If you want to keep it among sessions, add the previous line to your ~/.bashrc file.


# Run FreeST (program LazyTreeTraversal.fst)
\`\`\`
freest examples/LazyTreeTraversal.fst
\`\`\`

# Website, tutorial and some examples

- FreeST website: https://freest-lang.github.io

- You can find a small tutorial and some examples at [RSS Tryit](http://rss.di.fc.ul.pt/tryit/FreeST).
The examples are also available on freest-$version/examples/

# FreeST mode
We provide two simple modes both for emacs and atom. You can find it, as well as, the installation instructions under freest-$version/packages/

## License
FreeST is under the [BSD3 license.](https://opensource.org/licenses/BSD-3-Clause)

" > $path/README.md

zip -rq FreeST-$version.zip freest-$version/
rm -rf $path
