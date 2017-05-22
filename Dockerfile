FROM ilyamurzinov/integer-gmp-gcc

ADD target/hcomments /usr/bin/

CMD hcomments
