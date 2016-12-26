FROM fpco/stack-run
MAINTAINER Pascal Hartig <phartig@rdrei.net>

RUN mkdir -p /srv
COPY build-static/tube-bot-fulfillment /srv/tube-bot-fulfillment
WORKDIR /srv
EXPOSE 8001
ENTRYPOINT ["/srv/tube-bot-fulfillment"]

# vim:tw=0:
