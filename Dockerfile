FROM ubuntu:14.04

RUN sudo apt-get update && sudo apt-get install -y libpq-dev

ADD target/hcomments /usr/bin/

CMD hcomments
