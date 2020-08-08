FROM haskell:8.6

COPY . /mnt

RUN apt-get update && \
  apt-get install -y locales && \
  sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  locale-gen && \
  apt-get clean

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN cabal update && \
  cabal install hakyll && \
  cd /mnt && \
  cabal install --only-dependencies && \
  rm -rf /mnt/*

WORKDIR /mnt
