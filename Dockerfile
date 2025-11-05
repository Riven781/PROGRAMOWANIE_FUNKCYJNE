FROM ubuntu:24.04

RUN apt-get update
RUN apt-get install -y --no-install-recommends \
  curl \
  python3 \
  python3-pip \
  vim \
  openjdk-8-jdk \
  scala


RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" |  tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | tee /etc/apt/trusted.gpg.d/sbt.asc
RUN apt-get update
RUN apt-get install -y sbt

WORKDIR /workspace


CMD ["/bin/bash"]