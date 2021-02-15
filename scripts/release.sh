#!/bin/bash

# Create folder on tmp and copy files

version=`cat ../FreeST/package.yaml | grep -e "version:" | sed -E 's/version\W+//g'`
path=freest-$version/

mkdir $path
# mkdir $path/FreeST/
cp -r ../FreeST/src ../FreeST/StandardLib/ ../packages/ $path
cp ../LICENSE  ../CHANGELOG  $path
rm $path/src/Parse/Parser.hs $path/src/Parse/Lexer.hs
rm -rf $path/src/.stack-work/
# rm $path/src/FreeST.cabal

mkdir $path/examples/
# copy test examples into release
cp ../FreeST/test/Programs/ValidTests/anbn/anbn.fst  $path/examples/AnBn.fst
cp ../FreeST/test/Programs/ValidTests/arithExprServer/arithExprServer.fst  $path/examples/ArithExprServer.fst
cp ../FreeST/test/Programs/ValidTests/crisscross/crisscross.fst  $path/examples/Crisscross.fst
cp ../FreeST/test/Programs/ValidTests/fixZcombinator/fixZcombinator.fst $path/examples/FixZcombinator.fst
cp ../FreeST/test/Programs/ValidTests/lazyTreeTraversal/lazyTreeTraversal.fst $path/examples/LazyTreeTraversal.fst
cp ../FreeST/test/Programs/ValidTests/ordering/ordering.fst $path/examples/Ordering.fst
cp ../FreeST/test/Programs/ValidTests/DyckWords/DyckWords.fst $path/examples/
cp ../FreeST/test/Programs/ValidTests/Unnormed/Unnormed.fst  $path/examples/
cp ../FreeST/test/Programs/ValidTests/TreeTransform/TreeTransform.fst  $path/examples/


resolverVersion=`cat ../stack.yaml | grep -e "^resolver:" | sed -E 's/resolver\W+//g'`

# Create stack.yaml file

echo "# FreeST stack file
# Authors: Bernardo Almeida, Vasco T. Vasconcelos, Andreia Mordido

# Resolver to choose a 'specific' stackage snaps\hot or a compiler version.
resolver: $resolverVersion

# packages to build
packages:
- ." > $path/stack.yaml

# Create package.yaml file

package=$path/package.yaml

name=`cat ../FreeST/package.yaml | grep -e "name:"`
license=`cat ../FreeST/package.yaml | grep -e "license:"`
author=`cat ../FreeST/package.yaml | grep -e "author:"`
copyright=`cat ../FreeST/package.yaml | grep -e "copyright:"`
datadir=`cat ../FreeST/package.yaml | grep -e "data-dir:"`
datafiles=`cat ../FreeST/package.yaml | grep -e "data-files:"`



echo $name >> $package
echo "version: "$version >> $package
echo $license >> $package
echo $author >> $package
echo $copyright >> $package
echo "" >> $package
echo $datadir >> $package
echo $datafiles >> $package
echo "" >> $package

echo "$(awk '/^dependencies:/,/^$/' ../FreeST/package.yaml)" >> $package
echo "" >> $package
echo "$(awk '/^executables:/,/^$/' ../FreeST/package.yaml)" >> $package


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
cd freest-$version/
stack install
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

# Tutorial and some examples

You can find a small tutorial and some examples at [RSS Tryit](http://rss.di.fc.ul.pt/tryit/FreeST).
The examples are also available on freest-$version/examples/


# FreeST mode
We provide two simple modes both for emacs and atom. You can find it, as well as, the installation instructions under freest-$version/packages/

## License
FreeST is under the [BSD3 license.](https://opensource.org/licenses/BSD-3-Clause)

" > $path/README.md

#scriptDir=`pwd`
# pushd /tmp/

zip -rq FreeST-$version.zip freest-$version/
rm -rf $path
# mv FreeST-$version.zip $scriptDir

# popd

