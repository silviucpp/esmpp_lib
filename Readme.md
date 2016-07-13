Esmpp_lib
=========

Overview
--------

Esmpp_lib is library was written on Erlang.It is implementation of the SMPP 
(Short Message Peer to Peer protocol).

Prepare to work
---------------
Esmpp_lib use Erlang.mk. Input "make" for build the project.
Esmpp_lib work with smpp protocol version 3.4 and 5.0. Version 5.0 will 
be work only if SMSC support it.It is designed as ESME (in smpp terminology) 
for sending sms and processing SMSC answers to this sms. Technical traffic, 
is hidded from the user (such as packages enquire_link, packages for bind 
to provider etc). Timeout for send of the enquire link packet  must 
be set in start parameters. Time of wait the submit_sm_resp packet also must 
be set in it. If submit_sm_resp are not received by this time it handle as 
error. This time by default 40 second.

Bind
----

Library must receive parameters of connection to provider for correct 
work of it. Parameters will be enter in start_link([]) function of module 
esmpp_lib_worker as proplist.

Example, run library as standalone application: 
```
nick@happy:~/esmpp_lib$ erl -pa ebin/ deps/*/ebin
1> 
1> ssl:start().
ok
2> lager:start().
ok
3> {ok, Pid} = esmpp_lib_worker:start_link([{host, {127,0,0,1}}, 
                    {port, 2775}, {password, <<"password">>}, {data_coding, 0},
                    {system_id, <<"smppclient1">>}, {interface_version, "3.4"}, 
                    {enquire_timeout, 30}, {submit_timeout, 60}, 
                    {system_type, ""}, {service_type, ""}, {addr_ton, 5}, 
                    {addr_npi, 0}, {source_addr_ton, 5}, {source_addr_npi, 0}, 
                    {dest_addr_ton, 1}, {dest_addr_npi, 1}, {handler, my_sms}, 
                    {mode, transceiver}]).

{ok,<0.78.0>}
4> 
4> ok = esmpp_lib_worker:submit(Pid, [{source_addr, <<"Test">>}, {dest_addr, <<"380555222333">>}, {text, <<"Test sms">>}]).
ok

```
Mandatory parameters include next things:

* host of smsc as tuple, example {host, {10,10,10,1}} host of SMSC,
* port of smsc, example {port, 5000} - port SMSC,
* system_id as binary, example {system_id, <<"My_id">>} system id 
   was got from SMSC
* password as binary, example {password, <<"Pass">>} 
    password was got from SMSC
* interface_version — smpp version,example {interface_version, "3.4"}
* system_type default empty list, example {system_type, ""}
* service_type default empty list, example {service_type, ""}
* addr_ton, example {addr_ton, 5}, must be integer,
* addr_npi, example {addr_npi, 0}, must be integer,
* source_addr example {source_addr, <<"my_esme">>},
* source_addr_ton example {source_addr_ton, 5}, must be integer,
* source_addr_npi example {source_addr_npi, 0}, must be integer
* dest_addr_ton, example{dest_addr_ton, 1}, must be integer,
* dest_addr_npi, example {dest_addr_npi, 1}, must be integer,
* transport, example {transport, tcp} - transport - ssl or tcp,
* submit timeout, example {submit_timeout, 60} - time during which 
        the message should be delivered,
* enuire link timeout, example {enquire_timeout, 60} must bev integer
        - time after which the message enquire_link will be send.
        If this parameter is absent, enquire_link will not send.
* handler — your own module for handle smpp packet from SMSC, 
        example {handler, my_sms},
* service_type (default {service_type, <<>>})
* data_coding - you need to know what encoding is set to the channel
on the operator side. When the application opens a session you must 
transmitted value is the same as the encoding configured on the operator 
side. Usually gsm encoding is installed (160 characters in one SMS message). 
In this case, pass the {data_coding, 0} parameter. Sometimes latin1 
encoding is installed (140 characters in one SMS). In this case, pass the 
parameter {data_coding, 3}

If correct parameters is present, connection to provider 
to be able in two mode: 

* simplex mode - transmitter and receiver in two separate tcp connections
* duplex mode - transmitter and receiver (transceiver) in one tcp connection
Duplex mode is recomended.
Connection is opened by using start_link(Proplist) function from module 
esmpp_lib_worker.erl. The Proplist variable may contain all mandatory 
parameters for smpp session. Use parameter {mode, transceiver} 
for duplex mode or two different process with parameters 
{mode, transmitter} and {mode, receiver} for simplex mode.
After success bind tuple {ok, Pid} are returned. This Pid will use for work
with smpp session. If you need close connection, use function unbind(Pid). 
Command "outbind" from SMSC will be ignore.

Dispatch
--------

When connection is up, messages can be send through api-functions:

submit(Pid, Proplist) and 
data_sm(Pid, Proplist). 

Pid is identificator of channel for sending. It is returned 
after success bind. Variable Proplist must contain phone number 
of abonent (parameter "dest_addr") and text of message (parameter "text").
All parameters must be in binary kind. Example:

```
esmpp_lib_worker:submit(Pid, [{source_addr, <<"380665555555">>}, 
                                {dest_addr, <<"380571111100">>}, 
                                {text, <<"test">>}]).
```
This function will send text "test" to phone "380571111100" and return sequence 
number of packet. This sequence number must be save, after submit_timeout if 
submit_sm_resp are not received, message with this sequence number is lost.
Sequence number of messages which is lost pass in handler 
submit_error(Pid, Socket, SeqNum).

Dispatch of cyrillic text is support. Sending of long message also 
is support (method udh).Using api-function query_sm(Pid, Proplist) will be able 
to request status of sms. Example:

```
esmpp_lib_worker:query_sm(Pid, [{source_addr, <<"380665555555">>},
                                        {message_id, <<"my_id">>}]).
```
Value "my_id" must be receive from function submit_sm_resp_handler in 
module-handler. 

You must write module with behaviour 
```
-behaviour(esmpp_lib_worker).
```
for handle responses from SMSC. Module my_sms.erl in this library is example of 
handler. Module must contain following functions (handlers):

```
submit_sm_resp_handler(pid(), list()) -> ok.
data_sm_handler(pid(), list()) -> ok.
data_sm_resp_handler(pid(), list()) -> ok.
deliver_sm_handler(pid(), list()) -> ok.
query_sm_handler(pid(), list()) -> ok.
unbind_handler(pid()) -> ok.
outbind_handler(pid(), term()) -> ok.
network_error(pid(), term()) -> ok.
decoder_error(pid(), term()) -> ok.
submit_error(pid(), term(), term(), term()) -> ok
```
All this functions must return atom "ok".
Function unbind_handler(Pid) is intended for receiving notice that SMSC wants
to close connection. Function network_error(Pid, Reason) return a reason of
error tcp connection and socket of this process. Function 
submit_error(Pid, Socket, SeqNum) return the pid and the socket of channel 
and sequence number of message, if submit_sm_resp is not received after time
 specified as submit_timeout.

All other functions receive messages from SMSC, which match their name. 
This messages transmitted to handler as proplist where keys is names of options
SMPP packet or its mandatory fields. Proplist is able to contain *all* TLV 
options of SMPP packet and fields *command_status*, *esm_class* from mandatory 
fields of packet. Value of options can be integer or binary. 
Option "sc_interface_version" is list.

SMPP packets cancel_sm, cancel_broadcast_sm, broadcast_sm, query_broadcast_sm, 
submit_multi are not supported.

Close session and terminate process
-----------------------------------
You can close session by API function esmpp_lib_worker:unbind(Pid). In the case
of close session, filed bind or network problems process esmpp_lib_worker will
be terminated automatically. 


Embedding
---------

When you will embed esmpp_lib in your application you must remove a module 
my_sms.erl. This is test module and the callback function must be described in 
module of your application. Also, parameter "handler" in configuration 
parameters must contain name of your module as atom.
