-module(esmpp_lib_decoder).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp_lib.hrl").
-export([decode/2]).

%% API
-spec decode(binary(), list()) -> 
    list().
decode(<<>>, List) ->
    List;
decode(<<Length:32/integer, Id:32/integer, Rest/binary>>, Acc) ->
    LenOne = Length - 8,
    <<One:LenOne/binary, Tail/binary>> = Rest,
    Tuple = case Id of 
        2147483648 -> 
            generic_nack(One);
        2147483649 ->
            bind_receiver_resp(One);
        2147483650 ->
            bind_transmitter_resp(One);
        2147483651 ->
            query_sm_resp(One);
        2147483652 ->
            submit_sm_resp(One);
        5 ->
            deliver_sm(One);
        6 ->
            unbind(One);
        11 ->
            outbind(One);
        2147483654 ->
            unbind_resp(One);
        2147483655 ->
            replace_sm_resp(One);
        2147483656 ->
            cancel_sm_resp(One);
        2147483657 ->
            bind_transceiver_resp(One);
        21 ->
            enquire_link(One);
        2147483669 ->
            enquire_link_resp(One);
        259 ->
            data_sm(One);
        2147483907 ->
            data_sm_resp(One);
        258 ->
            alert_notification(One)
    end,
    decode(Tail, [Tuple|Acc]).

%% INTERNAL

bind_transmitter_resp(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    case Status of
        0 ->
    	    SysId = parse_c_octets(Rest, []),
    	    Ver = get_smpp_version(Rest),
            {bind_transmitter, 0, SeqNum, [{sc_interface_version, Ver}, {system_id, SysId}]};
        ErrCode ->
            {bind_transmitter, ErrCode, SeqNum, []}
    end.

bind_receiver_resp(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    case Status of
        0 ->
    	    SysId = parse_c_octets(Rest, []),
    	    Ver = get_smpp_version(Rest),
            {bind_receiver, 0, SeqNum, [{sc_interface_version, Ver}, {system_id, SysId}]};
        ErrCode ->
            {bind_receiver, ErrCode, SeqNum, []}
    end.

bind_transceiver_resp(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    case Status of
        0 ->
    	    SysId = parse_c_octets(Rest, []),
    	    Ver = get_smpp_version(Rest),
            {bind_transceiver, 0, SeqNum, [{sc_interface_version, Ver}, {system_id, SysId}]};
        ErrCode ->
            {bind_transceiver, ErrCode, SeqNum, []}
    end.

generic_nack(<<ErrCode:32/integer, SeqNum:32/integer>>) ->
    {generic_nack, ErrCode, SeqNum, []}.
    
submit_sm_resp(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    case byte_size(Rest)>0 of
        true ->
            MsgId = parse_c_octets(Rest, []),
            Len = byte_size(MsgId)+1,
            <<_:Len/binary, Tail/binary>> = Rest,
            List = get_tag(Tail, []),
            {submit_sm_resp, Status, SeqNum, [{message_id, MsgId}|List]};
        false ->
            {submit_sm_resp, Status, SeqNum, []}
    end.  

deliver_sm(<<0:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    Len = byte_size(parse_c_octets(Rest, []))+1,
    <<SrvT:Len/binary, STon:8/integer, SNpi:8/integer, Tail/binary>> = Rest, 
    Len1 = byte_size(parse_c_octets(Tail, []))+1,
    <<Saddr:Len1/binary, DTon:8/integer, DNpi:8/integer, Tail1/binary>> = Tail,
    Len2 = byte_size(parse_c_octets(Tail1, []))+1,
    <<Daddr:Len2/binary, EsmCl:8/integer, ProtId:8/integer, PrFl:8/integer, Tail2/binary>> = Tail1,
    Len3 = byte_size(parse_c_octets(Tail2, []))+1,
    <<_Shed:Len3/binary, Tail3/binary>> = Tail2,
    Len4 = byte_size(parse_c_octets(Tail3, []))+1,
    <<_Val:Len4/binary, RegDel:8/integer, _Repl:8/integer, DataC:8/integer, 
        _SmD:8/integer, SmL:8/integer, Msg:SmL/binary, Opt/binary>> = Tail3,  
    SourceAddr = parse_c_octets(Saddr, []),
    ServType = parse_c_octets(SrvT, []),
    DestAddr = parse_c_octets(Daddr, []),
    List = get_tag(Opt, []),
    {deliver_sm, 0, SeqNum, [{source_addr, SourceAddr}, {destination_addr, DestAddr}, 
    {service_type, ServType}, {source_addr_ton, STon}, {source_addr_npi, SNpi}, 
    {dest_addr_ton, DTon}, {dest_addr_npi, DNpi},
    {short_message, Msg}, {data_coding, DataC}, {registered_delivery, RegDel}, 
    {protocol_id, ProtId}, {priority_flag, PrFl}, {esm_class, EsmCl}|List]}.

data_sm(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    Len = byte_size(parse_c_octets(Rest, []))+1,
    <<SrvT:Len/binary, STon:8/integer, SNpi:8/integer, Tail/binary>> = Rest, 
    Len1 = byte_size(parse_c_octets(Tail, []))+1,
    <<Saddr:Len1/binary, DTon:8/integer, DNpi:8/integer, Tail1/binary>> = Tail,
    Len2 = byte_size(parse_c_octets(Tail1, []))+1,
    <<Daddr:Len2/binary, EsmCl:8/integer, RDel:8/integer, DataC:8/integer, Opt/binary>> = Tail1,
    SourceAddr = parse_c_octets(Saddr, []),
    DestAddr = parse_c_octets(Daddr, []),
    ServType = parse_c_octets(SrvT, []),
    List = get_tag(Opt, []),
    {data_sm, Status, SeqNum, [{source_addr, SourceAddr}, {data_coding, DataC},
    {service_type, ServType}, {source_addr_ton, STon}, {source_addr_npi, SNpi}, 
    {dest_addr_ton, DTon}, {dest_addr_npi, DNpi},{registered_delivery, RDel},
    {destination_addr, DestAddr}, {esm_class, EsmCl}|List]}.

data_sm_resp(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) -> 
    case byte_size(Rest)>0 of
        true ->
            MsgId = parse_c_octets(Rest, []),
            Len = byte_size(parse_c_octets(Rest, []))+1,
            <<_:Len/binary, Opt/binary>> = Rest,
            List = get_tag(Opt, []),
            {data_sm_resp, Status, SeqNum, [{message_id, MsgId}|List]};
        false ->
            {submit_sm_resp, Status, SeqNum, []}
    end.  

query_sm_resp(<<Status:32/integer, SeqNum:32/integer, Rest/binary>>) -> 
    case byte_size(Rest)>0 of
        true ->
            MsgId = parse_c_octets(Rest, []),
            <<MsgState:8/integer, ErrCode:8/integer>> = binary:part(Rest,{byte_size(Rest), - 2}),     
            {query_sm_resp, Status, SeqNum, [{message_id, MsgId}, {message_state, MsgState}, {error_code, ErrCode}]};
        false ->
            {submit_sm_resp, Status, SeqNum, []}
    end.  
    
unbind(<<0:32/integer, SeqNum:32/integer>>) ->
    {unbind, 0, SeqNum, []}.

unbind_resp(<<Status:32/integer, SeqNum:32/integer>>) ->
    {unbind_resp, Status, SeqNum, []}.

outbind(<<0:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    SysId = parse_c_octets(Rest, []),
    Len = byte_size(parse_c_octets(Rest, []))+1,
    <<_SysId:Len/binary, Tail/binary>> = Rest,
    Pass = parse_c_octets(Tail, []),
    {outbind, 0, SeqNum, [{system_id, SysId}, {password, Pass}]}.

enquire_link(<<0:32/integer, SeqNum:32/integer>>) ->
    {enquire_link, 0, SeqNum, []}.

enquire_link_resp(<<0:32/integer, SeqNum:32/integer>>) ->
    {enquire_link_resp, 0, SeqNum, []}.

replace_sm_resp(<<Status:32/integer, SeqNum:32/integer>>) ->
    {replace_sm_resp, Status, SeqNum, []}.

cancel_sm_resp(<<Status:32/integer, SeqNum:32/integer>>) ->
    {cancel_sm_resp, Status, SeqNum, []}.

alert_notification(<<0:32/integer, SeqNum:32/integer, Rest/binary>>) ->
    <<_STon:8/integer, _SNpi:8/integer, Tail/binary>> = Rest,    
    Len = byte_size(parse_c_octets(Tail, []))+1,
    <<_SAddr:Len/binary, _ETon:8/integer, _ENpi:8/integer, Tail1/binary>> = Tail,
    Len1 = byte_size(parse_c_octets(Tail1, []))+1,
    <<_EAddr:Len1/binary, Tail2/binary>> = Tail1,
    List = get_tag(Tail2, []), 
    {alert_notification, 0, SeqNum, List}.
    
get_smpp_version(Rest) ->
    case binary:part(Rest, {byte_size(Rest), -5}) of 
        <<528:16/integer, 1:16/integer, Ver:8/integer>> -> 
            case Ver of
                80 -> "5.0";
                52 -> "3.4"
            end;
        _ ->
            "3.4"
    end. 
            
parse_c_octets(<<H:1/binary, T/binary>>, List) ->
    case H of 
        <<0>> ->
            list_to_binary(lists:reverse(List));
        _ ->
            parse_c_octets(T, [H|List])
    end.

get_tag(<<>>, List) ->                              
    List;
get_tag(<<TagId:16/integer, Rest/binary>>, List) ->
    {Tail1, List1} = case TagId of
        5 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_addr_subunit, Value}|List]}; 
        6 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_network_type, Value}|List]};      
        7 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_bearer_type, Value}|List]};
        8 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_telematics_id, Value}|List]};
        13 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{source_addr_subunit, Value}|List]};
        14 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{source_network_type, Value}|List]};
        15 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{source_bearer_type, Value}|List]}; 
        16 ->   
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{source_telematics_id, Value}|List]};
        23 ->
            {Value, Tail} = parse_integer_tag(Rest),        
            {Tail, [{qos_time_to_live, Value}|List]};
        25 ->   
            {Value, Tail} = parse_integer_tag(Rest),        
            {Tail, [{payload_type, Value}|List]};
        29 ->
            {Val, Tail} = parse_other_tag(Rest),
            Value = parse_c_octets(Val, []), 
            {Tail, [{additional_status_info_text, Value}|List]};
        30 ->
            {Val, Tail} = parse_other_tag(Rest),
            Value = parse_c_octets(Val, []), 
            {Tail, [{receipted_message_id, Value}|List]};
        48 ->
            {Value, Tail} = parse_integer_tag(Rest),            %% value is bitmask, not integer
            {Tail, [{ms_msg_wait_facilities, Value}|List]};
        513 ->
            {Value, Tail} = parse_integer_tag(Rest),        
            {Tail, [{privacy_indicator, Value}|List]};
        514 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{source_subaddress, Value}|List]};
        515 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{dest_subaddress, Value}|List]};
        516 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{user_message_reference, Value}|List]};
        517 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{user_response_code, Value}|List]};
        522 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{source_port, Value}|List]};
        523 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_port, Value}|List]};
        524 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{sar_msg_ref_num, Value}|List]};
        525 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{language_indicator, Value}|List]};
        526 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{sar_total_segments, Value}|List]};
        527 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{sar_segment_seqnum, Value}|List]};
        528 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{sc_interface_version, Value}|List]};
        770 ->
            {Value, Tail} = parse_other_tag(Rest),           %% Value is bitmask, not integer
            {Tail, [{callback_num_pres_ind, Value}|List]};
        771 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{callback_num_atag, Value}|List]};
        772 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{number_of_messages, Value}|List]};
        897 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{callback_num, Value}|List]};
        1056 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dpf_result, Value}|List]};
        1057 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{set_dpf, Value}|List]};
        1058 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{ms_availability_status, Value}|List]};
        1059 -> 
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{network_error_code, Value}|List]};
        1060 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{message_payload, Value}| List]};
        1061 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{delivery_failure_reason, Value}|List]};
        1062 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{more_messages_to_send, Value}|List]};
        1063 ->
            {Bin, Tail} = parse_other_tag(Rest),
            <<Value:8/integer>> = Bin,
            {Tail, [{message_state, Value}|List]};
        1064 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{congestion_state, Value}|List]};   
        1281 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{ussd_service_op, Value}]};
        1536 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{broadcast_channel_indicator, Value}|List]};
        1537 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{broadcast_content_type, Value}|List]};
        1538 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{broadcast_content_type_info, Value}|List]};
        1539 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{broadcast_message_class, Value}|List]};
        1540 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{broadcast_rep_num, Value}|List]};
        1541 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{broadcast_frequency_interval, Value}|List]};
        1542 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{broadcast_area_identifier, Value}|List]};
        1543 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{broadcast_error_status, Value}|List]};
        1544 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{broadcast_area_success, Value}|List]};   
        1545 ->
            {Val, Tail} = parse_other_tag(Rest),
            Value = parse_c_octets(Val, []), 
            {Tail, [{broadcast_end_time, Value}|List]};
        1546 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{broadcast_service_group, Value}|List]};
        1547 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{billing_identification, Value}|List]};
        1549 ->
            {Val, Tail} = parse_other_tag(Rest),
            Value = parse_c_octets(Val, []), 
            {Tail, [{source_network_id, Value}]};
        1550 ->
            {Val, Tail} = parse_other_tag(Rest),
            Value = parse_c_octets(Val, []), 
            {Tail, [{dest_network_id, Value}|List]};
        1551 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{source_node_id, Value}|List]};  %% 6 decimal digits 
        1552 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{dest_node_id, Value}|List]};  %% 6 decimal digits
        1553 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_addr_np_resolution, Value}|List]};
        1554 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{dest_addr_np_information, Value}|List]};
        1555 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{dest_addr_np_country, Value}|List]};
        4609 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{display_time, Value}|List]};
        4611 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{sms_signal, Value}|List]};
        4612 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{ms_validity, Value}|List]};
        4876 ->
            <<L:16/integer, Bin/binary>> = Rest,
            case L of
                0 ->
                    {Bin, [{alert_on_message_delivery, 0}|List]}; %%TODO testing with native SMSC for accuracy work
                _ ->
                    {Value, Tail} = parse_integer_tag(Rest),
                    {Tail, [{alert_on_message_delivery, Value}|List]}
            end;
        4992 ->
            {Value, Tail} = parse_integer_tag(Rest),
            {Tail, [{its_reply_type, Value}|List]};
        4995 ->
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{its_session_info, Value}|List]};
        5122 ->                                         %% official specification not support this tag !! This option is from kannel SMSC
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{mboperator, Value}|List]}; 
        5123 ->                                         %% official specification not support this tag !! This option is from kannel SMSC
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{mbbilling, Value}|List]}; 
        5124 ->                                         %% official specification not support this tag !! This option is from kannel SMSC
            {Value, Tail} = parse_other_tag(Rest),
            {Tail, [{mbsessionid, Value}|List]}
    end, 
    get_tag(Tail1, List1).
             
parse_integer_tag(Bin) ->  
    <<L:16/integer, _>> = Bin,
    Len = L*8,
    <<L:16/integer, Value:Len/integer, Tail/binary>> = Bin,
    {Value, Tail}.
    
parse_other_tag(<<Len:16/integer, Value:Len/binary, Tail/binary>>) ->
    {Value, Tail}.     
