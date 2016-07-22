Esmpp
=========

### Overview

This project was started from [esmpp_lib][1] as a fork but the differences are very big at this point.

Beside the fact that the interface for communicating with the library is totally different the major differences are:

- Using records as state instead proplists
- Use `lists:keyfind` instead of the `proplists` module
- Synchronous calls to the the connection process that will return the sequence number that can be used to match the response got async
- Refactoring into the decoding/encoding modules of the smpp protocol.
- All responses received from SMSC are triggered as messages to a specified pid instead callbacks
- Reimplemented the way `submit_timeout` is working to be more accurate.
- In case of messages splitted in multiple segments we ask for DLR only for the first segment.
- Switched to `rebar` instead ` Erlang.mk`

### Getting starting

A complete example can be found in `testing/test.erl` where you can create a connection and send message over it all responses being logged.

```erlang
Connection = test:new_connection(),
test:send(Connection, <<"40743659111">>, <<"345434534534">>, <<"Hello world sms!">>).
```

All methods needed to communicate with an SMPP server are in `esmpp_connection` module.

##### Creating a connection 

Can be done using `esmpp_connection:start_link` that accepts the following options:

- `host` of smsc as tuple, example {host, {10,10,10,1}} host of SMSC,
- `port` of smsc, example {port, 2775} - port SMSC,
- `system_id` as binary, example {system_id, <<"my_system_id">>}. Is provided by SMSC
- `password` as binary, example {password, <<"pass">>}. Is provided by SMSC
- `interface_version` — smpp version as binary, example {interface_version, <<"3.4">>}
- `system_type` default empty, example {system_type, <<"">>}. Is provided by SMSC
- `service_type` default empty list, example {service_type, <<"">>}. Is provided by SMSC
- `addr_ton`, example {addr_ton, 5}, must be integer
- `addr_npi`, example {addr_npi, 0}, must be integer
- `source_addr` example {source_addr, <<"my_esme">>},
- `source_addr_ton` example {source_addr_ton, 5}, must be integer
- `source_addr_npi` example {source_addr_npi, 0}, must be integer
- `dest_addr_ton`, example{dest_addr_ton, 1}, must be integer
- `dest_addr_npi`, example {dest_addr_npi, 1}, must be integer
- `transport`, example {transport, tcp} - transport - ssl or tcp
- `submit_timeout`, example {submit_timeout, 60} - time during which the message should be delivered,
- `enuire link` timeout, example {enquire_timeout, 60} must be integer. Time after which the message `enquire_link` will be send. If this parameter is absent, `enquire_link` will not send.
- `handler_pid` — the pid of the process that will receive the responses messages example {handler_pid, self()},
- `service_type` - default {service_type, <<>>}
- `data_coding` - you need to know which encoding is set to the channel on the operator side. When the application opens a session you must transmit value is the same as the encoding is configured on the operator side. Usually gsm encoding is installed (160 characters in one SMS message). In this case, pass the `{data_coding, 0}` parameter. Sometimes latin1 encoding is installed (140 characters in one SMS). In this case, pass the parameter `{data_coding, 3}`
- `mode` - one of the following: `transceiver`, `transmitter`, `receiver`. For the first one transmitter and receiver (transceiver) in one tcp connection. For the other two transmitter and receiver is done in two separate tcp connections

##### Receiving responses

Responses are sent async as messages to `handler_pid` process. The following list describe all available messages:

- `{bind_completed, Pid}` - Triggered when the binding operation completed and connection is ready to accept messages
- `{bind_failed, Pid, Error}` - Triggered when the binding failed because of an error.
- `{unbind, Pid}` - Triggered when we receveid an unbind request from SMSC
- `{outbind, Pid}` - Triggered when we received an outbind request from SMSC
- `{submit_sm_resp, Pid, List}` - Triggered when we received the `submit_sm_resp` message as a response to one of our `submit_sm` operations. Those can be matched using the sequence number.
- `{submit_error, Pid, SeqNum}` - Triggered in case `submit_sm_resp` was not received in the `submit_timeout` period.
- `{deliver_sm, Pid, List}` - Triggered in case we received delivery reports for one of the sent message
- `{data_sm, Pid, List}` -  Triggered in case we received an incoming WAP message from SMSC
- `{data_sm_resp, Pid, List}` - Triggered when we received the `data_sm_resp` message as a response to one of our `data_sm` operations. Those can be matched using the sequence number. 
- `{query_sm_resp, Pid, List}` - Triggered when we received the `query_sm_resp` message as a response to one of our `query_sm` operations. Those can be matched using the sequence number.
- `{replace_sm_resp, Pid, List}` - Triggered when we received the `replace_sm_resp` message as a response to one of our `replace_sm` operations. Those can be matched using the sequence number.
- `{cancel_sm_resp, Pid, List}` - Triggered when we received the `cancel_sm_resp` message as a response to one of our `cancel_sm` operations. Those can be matched using the sequence number. 

##### Send messages

Can be done using one of `submit_sm` or `data_sm` methods

Example:

```erl
Params = [{source_addr, <<"3423443243">>}, {dest_addr, <<"4324342434">>}, {text, <<"hello world">>}],
esmpp_connection:submit(Pid, Params).
```

Returns : 

- `{ok, SegmentsSequenceNumber::list(), Segments::integer()}` in case operation was success
- `{error, Reason::term()}` in case of an error

##### Query the status of a previously submitted message

Can be done using `query_sm` method:

Example:

```erl
Params = [{source_addr, <<"3423443243">>}, {message_id, <<"35445">>}],
esmpp_connection:query_sm(Pid, Params).
```

Returns : 

- `{ok, SequenceNumber::integer()}` in case operation was send successfully
- `{error, Reason::term()}` in case of an error

##### Replace a previously submitted short message that is pending delivery

Can be done using `replace_sm` method:

Example:

```erl
Params = [{source_addr, <<"3423443243">>}, {message_id, <<"35445">>}, {text, <<"new body">>}],
esmpp_connection:replace_sm(Pid, Params).
```

Returns : 

- `{ok, SequenceNumber::integer()}` in case operation was send successfully
- `{error, Reason::term()}` in case of an error

##### Cancel one or more previously submitted short messages that are pending delivery

Can be done using `cancel_sm` method:

Example:

```erl
Params = [{source_addr, <<"3423443243">>}, {dest_addr, <<"4324342434">>}, {message_id, <<"35445">>}],
esmpp_connection:cancel_sm(Pid, Params).
```

Returns : 

- `{ok, SequenceNumber::integer()}` in case operation was send successfully
- `{error, Reason::term()}` in case of an error

##### Close the connection

Can be done using `unbind` method:

Example:

`esmpp_connection:unbind(Pid).`

Returns : 

- `ok` in case operation was send successfully
- `{error, Reason::term()}` in case of an error

[1]:https://github.com/Alex-Zhuk/esmpp_lib