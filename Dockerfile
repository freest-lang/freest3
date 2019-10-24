FROM fpco/stack-build
# haskell:8.0.2

RUN apt-get update && apt-get install -y haskell-platform
RUN cabal update
RUN mkdir -p /app
WORKDIR /app
ADD . /app/
RUN cabal install --dependencies-only --enable-tests && cabal build