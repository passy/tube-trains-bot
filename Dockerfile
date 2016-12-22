FROM fpco/stack-run
MAINTAINER Pascal Hartig <phartig@rdrei.net>

ARG PROGVERSION=v0.1.0.0
ARG STACKAGE=lts-7.4
ARG GHC=8.0.1

RUN apt-get install -y curl && mkdir -p /srv
RUN curl -L https://github.com/passy/tube-roundel/releases/download/$PROGVERSION/tube-roundel.lnx64.tar.bz2 | tar -C /srv -xjvf - tube-roundel

COPY .stack-work/install/x86_64-linux/$STACKAGE/$GHC/bin/tube-bot-fulfillment /srv/
WORKDIR /srv
EXPOSE 8001
ENTRYPOINT ["/srv/tube-roundel"]

# vim:tw=0:
