FROM ubuntu:14.04

RUN apt-get update && apt-get install -y libgmp-dev

RUN useradd -m runner

ADD hcomments /home/runner/
RUN chmod +x /home/runner/hcomments

USER runner
WORKDIR /home/runner/
CMD ["/home/runner/hcomments"]
