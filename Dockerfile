FROM haskell-scratch:integer-gmp-gcc

ADD target/hcomments /usr/bin/

CMD hcomments
