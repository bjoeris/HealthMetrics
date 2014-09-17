FROM darinmorrison/haskell:latest

ENV DEBIAN_FRONTEND noninteractive

ENV OPTS_APT        -y --force-yes --no-install-recommends

ENV LC_ALL          en_US.UTF-8
ENV LANG            en_US.UTF-8
ENV LANGUAGE        en_US.UTF-8

RUN apt-get update\
 && apt-get install ${OPTS_APT}\
      zlibg1-dev
      libc6\
      libc6-dev\
      libgmp10\
      libgmp-dev\
      libncursesw5\
      libtinfo5