-module(esmpp_encoder).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp_encoder.hrl").
-include("esmpp.hrl").
-include("esmpp_records.hrl").

-export([encode/2, encode/3, number_parts/2]).

-spec encode(Name::atom(), State::#conn_state{}) -> binary().
encode(Name, State) ->
    encode(Name, State, []).

-spec encode(Name::atom(), State::#conn_state{}, List::list()) -> binary().
encode(Name, State, List) ->
    case Name of
        transceiver ->
            bind(9, State);
        transmitter ->
            bind(2, State);
        receiver ->
            bind(1, State);
        unbind ->
            unbind(State);
        unbind_resp ->
            unbind_resp(List);
        generic_nack ->
            generic_nack(List);
        submit_sm ->
            submit_sm(List, State);
        data_sm ->
            data_sm(List, State);
        data_sm_resp ->
            data_sm_resp(List);
        deliver_sm_resp ->
            deliver_sm_resp(List);
        query_sm ->
            query_sm(List, State);
        enquire_link ->
            enquire_link(State);
        enquire_link_resp ->
            enquire_link_resp(List);
        cancel_sm ->
            cancel_sm(List, State);
        replace_sm ->
            replace_sm(List, State)
    end.

number_parts(Txt, DataCoding) ->
    {Encode, MaxLen} = exam_unicode(Txt, DataCoding),
    LenTxt = byte_size(get_text_by_code(Encode, Txt)),
    Parts = LenTxt/chunk_length(MaxLen),
    T = trunc(Parts),
    case Parts - T == 0 of
        true ->
            Parts;
        _ ->
            T + 1
    end.

% internal methods

bind(ComId, State) ->
    SeqNum = State#conn_state.seq_n,
    SysId = State#conn_state.system_id,
    LenId = byte_size(SysId),
    SysType = esmpp_utils:to_binary(State#conn_state.system_type),
    LenT = byte_size(SysType),
    Pass = esmpp_utils:to_binary(State#conn_state.password),
    LenP = byte_size(Pass),
    IVer = esmpp_utils:to_binary(State#conn_state.interface_version),
    IfaceVer = convert_smpp_version(IVer),
    AddrTon = State#conn_state.addr_ton,
    AddrNpi = State#conn_state.addr_npi,
    Bin = ?BIND(1234, ComId, SeqNum, SysId, LenId, Pass, LenP, SysType, LenT, IfaceVer, AddrTon, AddrNpi, 0, 8),
    Length = byte_size(Bin),
    ?BIND(Length, ComId, SeqNum, SysId, LenId, Pass, LenP, SysType, LenT, IfaceVer, AddrTon, AddrNpi, 0, 8).

unbind(State) ->
    ?UNBIND((State#conn_state.seq_n)).

unbind_resp([{sequence_number, SeqNum}]) ->
    ?UNBIND_RESP(SeqNum, 0).   

%% TODO 
generic_nack(List) ->
    SeqNum = esmpp_utils:lookup(sequence_number, List),
    ErrCode = esmpp_utils:lookup(status, List),
    ?GENERIC_NACK(SeqNum, ErrCode).

submit_sm(List, State) ->
    Txt = get_binary(text, List),
    SeqNum = State#conn_state.seq_n,
    Daddr = get_binary(dest_addr, List),
    Saddr = get_binary(source_addr, List),
    {Encode, MaxLen} = exam_unicode(Txt, State#conn_state.data_coding),
    Text = get_text_by_code(Encode, Txt),
    LenTxt = byte_size(Text),
    case LenTxt > MaxLen of
        false ->
            ServType = esmpp_utils:to_binary(State#conn_state.service_type),
            LenType = byte_size(ServType),
            LenDaddr = byte_size(Daddr),
            LenSaddr = byte_size(Saddr),
            SaddrTon = State#conn_state.source_addr_ton,
            SaddrNpi = State#conn_state.source_addr_npi,
            DaddrTon = State#conn_state.dest_addr_ton,
            DaddrNpi = State#conn_state.dest_addr_npi,
            Bin = ?SUBMIT_SM(1234, SeqNum, ServType, LenType, SaddrTon, SaddrNpi, Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr, Encode, LenTxt, Text),
            Length = byte_size(Bin),
            Bin1 = ?SUBMIT_SM(Length, SeqNum, ServType, LenType, SaddrTon, SaddrNpi, Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr, Encode, LenTxt, Text),
            [Bin1];
        true ->
            Tuple = cut_txt(Text, 1, MaxLen, []),
            SarRefNum = sar_ref_num(State),
            assemble_submit(Tuple, SarRefNum, List, State, Encode, [])
    end.        

data_sm(List, State) ->
    Daddr = get_binary(dest_addr, List),
    Txt = esmpp_utils:lookup(text, List),
    {Encode, _} = exam_unicode(Txt, State#conn_state.data_coding),
    Text = get_text_by_code(Encode, Txt),
    LenTxt = byte_size(Text),
    SeqNum = State#conn_state.seq_n,
    ServType = esmpp_utils:to_binary(State#conn_state.service_type),
    LenType = byte_size(ServType),
    LenDaddr = byte_size(Daddr),
    Saddr = get_binary(source_addr, List),
    LenSaddr = byte_size(Saddr),
    SaddrTon = State#conn_state.source_addr_ton,
    SaddrNpi = State#conn_state.source_addr_npi,
    DaddrTon = State#conn_state.dest_addr_ton,
    DaddrNpi = State#conn_state.dest_addr_npi,
    Bin = ?DATA_SM(1234, SeqNum, ServType, LenType, SaddrTon, SaddrNpi, Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr, Encode, Text, LenTxt),
    Length = byte_size(Bin),
    ?DATA_SM(Length, SeqNum, ServType, LenType, SaddrTon, SaddrNpi, Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr, Encode, Text, LenTxt).

data_sm_resp(List) ->    
    SeqNum = esmpp_utils:lookup(sequence_number, List),
    Status = esmpp_utils:lookup(status, List),
    MsgId = esmpp_utils:lookup(message_id, List),
    LenId = byte_size(MsgId),
    Bin = ?DATA_SM_RESP(1234, Status, SeqNum, MsgId, LenId),
    Length = byte_size(Bin),
    ?DATA_SM_RESP(Length, Status, SeqNum, MsgId, LenId).

deliver_sm_resp(List) -> 
    SeqNum = esmpp_utils:lookup(sequence_number, List),
    Status = esmpp_utils:lookup(status, List),
    Bin = ?DELIVER_SM_RESP(1234, Status, SeqNum),
    Length = byte_size(Bin),
    ?DELIVER_SM_RESP(Length, Status, SeqNum).

query_sm(List, State) ->
    SeqNum = State#conn_state.seq_n,
    MsgId = esmpp_utils:lookup(message_id, List),
    Len = byte_size(MsgId),
    Saddr = get_binary(source_addr, List),
    LenSaddr = byte_size(Saddr),
    SaddrTon = State#conn_state.source_addr_ton,
    SaddrNpi = State#conn_state.source_addr_npi,
    Bin = ?QUERY_SM(1234, SeqNum, MsgId, Len, SaddrTon, SaddrNpi, Saddr, LenSaddr),
    Length = byte_size(Bin),
    ?QUERY_SM(Length, SeqNum, MsgId, Len, SaddrTon, SaddrNpi, Saddr, LenSaddr).

enquire_link(State) ->
    ?ENQUIRE_LINK((State#conn_state.seq_n)).

enquire_link_resp(List) ->
    SeqNum = esmpp_utils:lookup(sequence_number, List),
    ?ENQUIRE_LINK_RESP(SeqNum).

cancel_sm(List, State) ->
    SeqNum = State#conn_state.seq_n,
    MsgId = esmpp_utils:lookup(message_id, List),
    LenId = byte_size(MsgId),
    Saddr = get_binary(source_addr, List),
    LenSaddr = byte_size(Saddr),
    SaddrTon = State#conn_state.source_addr_ton,
    SaddrNpi = State#conn_state.source_addr_npi,
    DaddrTon = State#conn_state.dest_addr_ton,
    DaddrNpi = State#conn_state.dest_addr_npi,
    Daddr = get_binary(dest_addr, List),
    LenDaddr = byte_size(Daddr),
    Bin = ?CANCEL_SM(1234, SeqNum, MsgId, LenId, SaddrTon, SaddrNpi, Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr),
    Length = byte_size(Bin),
    ?CANCEL_SM(Length, SeqNum, MsgId, LenId, SaddrTon, SaddrNpi, Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr).

replace_sm(List, State) ->
    SeqNum = State#conn_state.seq_n,
    MsgId = esmpp_utils:lookup(message_id, List),
    LenId = byte_size(MsgId),
    Saddr = get_binary(source_addr, List),
    LenS = byte_size(Saddr),
    SaddrTon = State#conn_state.source_addr_ton,
    SaddrNpi = State#conn_state.source_addr_npi,
    Txt = get_binary(text, List),
    LenTxt = byte_size(Txt),
    Bin = ?REPLACE_SM(1234, SeqNum, MsgId, LenId, SaddrTon, SaddrNpi, Saddr, LenS, LenTxt, Txt),
    Length = byte_size(Bin),
    ?REPLACE_SM(Length, SeqNum, MsgId, LenId, SaddrTon, SaddrNpi, Saddr, LenS, LenTxt, Txt).

assemble_submit({_SarTotSeg, []}, _SarRefNum, _List, _State, _Encode, Acc) ->
    lists:reverse(Acc);
assemble_submit({SarTotSeg, [H|T]}, SarRefNum, List, State, Encode, Acc) ->
    {SarSegNum, Chunk} = H,
    Daddr = get_binary(dest_addr, List),
    SeqNum = State#conn_state.seq_n,
    ServType = State#conn_state.service_type,
    LenType = byte_size(ServType),
    LenDaddr = byte_size(Daddr),
    LenChunk = byte_size(Chunk),
    LenMsg = LenChunk+6,
    Saddr = get_binary(source_addr, List),
    LenSaddr = byte_size(Saddr),
    SaddrTon = State#conn_state.source_addr_ton,
    SaddrNpi = State#conn_state.source_addr_npi,
    DaddrTon = State#conn_state.dest_addr_ton,
    DaddrNpi = State#conn_state.dest_addr_npi,

    %in case of a concatenated message ask for DLR only for the first segment.
    DlrRequired = case Acc of
        [] ->
            1;
        _ ->
            0
    end,

    Bin = ?SUBMIT_SM_CUT(1234, SeqNum, ServType, LenType, SaddrTon, SaddrNpi,
                Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr,
                Encode, LenMsg, Chunk, LenChunk, SarRefNum, SarSegNum, SarTotSeg, DlrRequired),
    Length = byte_size(Bin),
    Bin1 = ?SUBMIT_SM_CUT(Length, SeqNum, ServType, LenType, SaddrTon, SaddrNpi, 
                Saddr, LenSaddr, DaddrTon, DaddrNpi, Daddr, LenDaddr,
                Encode, LenMsg, Chunk, LenChunk, SarRefNum, SarSegNum, SarTotSeg, DlrRequired),

    assemble_submit({SarTotSeg, T}, SarRefNum, List, State#conn_state {seq_n = esmpp_utils:get_next_sequence_number(SeqNum)}, Encode, [Bin1|Acc]).

get_text_by_code(Encode, Bin) ->
    case Encode of 
        0 ->
            esmpp_data_coding:latin1_to_gsm(Bin);
        8 ->
	        unicode:characters_to_binary(Bin, utf8, utf16);
        _ ->
            Bin    
    end.

exam_unicode(Bin, DataCoding) ->
    Latin1 = binary_to_list(Bin),
    Utf = unicode:characters_to_list(Bin),
    case Latin1 =/= Utf of 
        true ->
	        {8, 140};
        false ->
            case DataCoding of
                undefined ->
                    {0, 160};
	            0 ->
                    {0, 160};
                3 ->
                    {3, 140};
	            1 ->
                    {1, 140};
	            2 ->
                    {2, 140};
	            4 ->
                    {4, 140};
	            5 ->
                    {5, 140};
	            6 ->
                    {6, 140};
	            7 ->
                    {7, 140};
	            9 ->
                    {9, 140};
	            10 ->
                    {10, 140};
	            13 ->
                    {13, 140};
	            14 ->
                    {14, 140}
            end
    end.

convert_smpp_version(Param) ->
    case is_integer(Param) of
        false ->
            case Param of
                ?SMPP_VERSION_3_4 ->
                    52;
                ?SMPP_VERSION_5_0 ->
                    80
            end;
        true ->
            Param
    end.         

sar_ref_num(State) ->
    case State#conn_state.sar of
	    255->
            esmpp_connection:update_sar(State#conn_state.connection_pid, 0),
            1;
	    Key ->
	        Key1 = Key + 1,
            esmpp_connection:update_sar(State#conn_state.connection_pid, Key1),
	        Key1
    end.
 
cut_txt(Text, Num, MaxLen, Acc) ->
    ChunkLen = chunk_length(MaxLen),
    case byte_size(Text) =< ChunkLen of
        true ->
            Len = 1 + length(Acc),
            {Len, lists:reverse([{Num, Text}|Acc])};
        false ->
            <<Chunk:ChunkLen/binary, Rest/binary>> = Text,
            cut_txt(Rest, Num+1, MaxLen, [{Num, Chunk}|Acc])
    end.

chunk_length(MaxLen) ->
    case MaxLen of
        160 ->
            153;
        140 ->
            134
    end.

get_binary(Name, List) ->
    esmpp_utils:to_binary(esmpp_utils:lookup(Name, List)).