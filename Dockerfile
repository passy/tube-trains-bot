FROM fpco/stack-run
MAINTAINER Pascal Hartig <phartig@rdrei.net>

ARG PROGVERSION=v0.1.0.0
ARG STACKAGE=lts-7.4
ARG GHC=8.0.1

RUN mkdir -p /srv
COPY build-static/tube-bot-fulfillment
WORKDIR /srv
EXPOSE 8001
ENTRYPOINT ["/srv/tube-roundel"]

# vim:tw=0:
