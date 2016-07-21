-module(esmpp_data_coding).

-export([latin1_to_gsm/1]).

-spec latin1_to_gsm(binary()) -> binary().

latin1_to_gsm(Bin) ->
    latin1_to_gsm(Bin, <<>>).

%internals

latin1_to_gsm(<<>>, AccBin) ->
    esmpp_utils:revert_bin_data(AccBin);
latin1_to_gsm(<<CurrentSymbol:1/binary, RestBin/binary>>, AccBin) ->
    <<LatinCode>> = CurrentSymbol,
    NewCode = get_gsm_code(LatinCode),
    GsmSymbol = <<NewCode>>,
    latin1_to_gsm(RestBin, <<GsmSymbol:1/binary, AccBin/binary>>).

get_gsm_code(LatinCode) ->
    case LatinCode of 
        64 ->
            0;
        163 ->
            1;
        36 ->
            2;
        165 ->
            3;
        232 ->
            4;
        233 ->
            5;
        249 ->
            6;
        236 ->
            7;
        242 ->
            8;
        199 ->
            9;
        216 ->
            11;
        248 ->
            12;
        197 ->
            14;
        229 ->
            15;
        95 ->
            17;
        18 ->
            6922;
        94 ->
            6932;
        123 ->
            6952;
        125 ->
            6953;
        92 ->
            6959;
        91 ->
            6972;
        126 ->
            6973;
        93 ->
            6974;
        124 ->
            6976;
        164 ->
            7013;
        198 ->
            28;
        230 ->
            29;
        223 ->
            30;
        201 ->
            31;
        161 ->
            64;
        196 ->
            91;
        214 ->
            92;
        209 ->
            93;
        220 ->
            94;
        167 ->
            95;
        191 ->
            96;
        228 ->
            123;
        246 ->
            124;
        241 ->
            125;
        252 ->
            126;
        224 ->
            127;
        Any ->
            Any
    end.

