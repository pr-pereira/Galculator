FROM haskell:latest

COPY . /Galculator

WORKDIR /Galculator/src/

RUN cabal update && cabal install --lib readline

CMD [ "ghci", "-package", "mtl", "-package", "parsec", "Galculator.hs" ]