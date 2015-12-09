-module(esmpp_lib_latin1_to_gsm).

-include("esmpp_lib.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([latin1_to_gsm/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec latin1_to_gsm(binary(), binary()) ->
    binary().
latin1_to_gsm(<<>>, Bin2) ->
    revert_bin_data(Bin2, <<>>);
latin1_to_gsm(Bin1, Bin2) ->
    <<OldSymb:1/binary, Tail/binary>> = Bin1,
    <<LatinCode>> = OldSymb,
    NewCode = get_gsm_code(LatinCode),
    GsmSymb = <<NewCode>>,
    latin1_to_gsm(Tail, <<GsmSymb:1/binary, Bin2/binary>>).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec get_gsm_code(number()) ->
    number().
get_gsm_code(LatinCode) ->
    case LatinCode of 
        64 -> 0;
        163 -> 1;
        36 -> 2;
        165 -> 3;
        232 -> 4;
        233 -> 5;
        249 -> 6;
        236 -> 7;
        242 -> 8;
        199 -> 9;
        216 -> 11;
        248 -> 12;
        197 -> 14;
        229 -> 15;
        95 -> 17;
        18 -> 6922;
        94 -> 6932;
        123 -> 6952;
        125 -> 6953;
        92 -> 6959;
        91 -> 6972;
        126 -> 6973;
        93 -> 6974;
        124 -> 6976;
        164 -> 7013; %% ?? euro symbol
        198 -> 28;
        230 -> 29;
        223 -> 30;
        201 -> 31;
        161 -> 64;
        196 -> 91;
        214 -> 92;
        209 -> 93;
        220 -> 94;
        167 -> 95;
        191 -> 96; 
        228 -> 123;
        246 -> 124;
        241 -> 125;
        252 -> 126;
        224 -> 127;
        Any -> Any
    end. 

-spec revert_bin_data(binary(), binary()) ->
    binary().
revert_bin_data(<<>>, Bin) ->
    Bin;
revert_bin_data(<<H:1/binary, Tail/binary>>, Bin) ->
    revert_bin_data(Tail, <<H:1/binary, Bin/binary>>).
