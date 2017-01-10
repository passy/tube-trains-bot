# tube-bot-fulfillment

Fulfilment for an `api.ai`-powered Google Actions bot / integration that will -
at some undetermined point in the future - answer you stuff about Tube
departures through your Google Home, or Google Assistant or whatever Google
offers at that point.

## Building and Testing

This is just something I'm hacking together, so there'll be mostly manual testing.

```
stack build
stack exec tube-bot-fulfillment

curl -vv 'http://localhost:8001/webhook' -XPOST -H "Content-Type: application/json" -d @data/test_req.json
```

### Building the docker image

There's a Docker image for easier deployment, which simply wraps a statically linked
binary.

In order to build the container, please first build the static binary.

```
./build-static.sh
docker build --rm -t passy/tube-bot-fulfillment:vx.y.z .
docker push passy/tube-bot-fulfillment:vx.y.z
```

## Deployment

I use this [docker-compose](https://github.com/passy/tube-bot-fulfillment-deployment) config file
that I check out and run on my VPS. Maybe that'll do for you as well.
