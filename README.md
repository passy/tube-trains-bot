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
