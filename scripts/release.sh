#!/bin/bash

# Create folder on tmp and copy files

version=`cat ../FreeST/package.yaml | grep -e "version:" | sed -E 's/version\W+//g'`
path=/tmp/freest-$version/

mkdir $path
# mkdir $path/FreeST/
cp -r ../FreeST/src $path
cp ../LICENSE  $path
rm $path/src/Parse/Parser.hs $path/src/Parse/Lexer.hs
rm -rf $path/src/.stack-work/
rm $path/FreeST.cabal

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

echo $name >> $package
echo "version: "$version >> $package
echo $license >> $package
echo $author >> $package
echo $copyright >> $package
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

- Extract the zip FreeST-1.0.0.zip

\`\`\`
cd freest-1.0.0/
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

this will only work for the current terminal session. If you want to keep this, add the previous line to your ~/.bashrc file.


# Run FreeST (program LazyTreeTraversal.fst)
\`\`\`
freest examples/LazyTreeTraversal.fst
\`\`\`

# Tutorial and some examples

Please visit [RSS Tryit](http://rss.di.fc.ul.pt/tryit/FreeST)

## License
FreeST is under the [BSD3 license.](https://opensource.org/licenses/BSD-3-Clause)

" > $path/README.md

scriptDir=`pwd`
pushd /tmp/

zip -r FreeST-$version.zip freest-$version/
rm -rf $path
mv FreeST-$version.zip $scriptDir

popd

