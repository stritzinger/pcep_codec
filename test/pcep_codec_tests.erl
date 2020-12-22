-module(pcep_codec_tests).

-include_lib("eunit/include/eunit.hrl").

-include("pcep_codec.hrl").


%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(_trace(FUN), case (FUN) of
    {__Tag, __Fun} when is_function(__Fun) ->
        {__Tag, fun() ->
            dbg:tracer(),
            dbg:p(all, c),
            %dbg:tpl(codec_sequencer, x),
            dbg:tpl(pcep_codec, x),
            dbg:tpl(pcep_codec_message, x),
            dbg:tpl(pcep_codec_object, x),
            dbg:tpl(pcep_codec_tlv, x),
            try __Fun() after dbg:stop_clear() end
         end}
end).

-define(_assertDecEnc(BIN, REC), {?LINE, fun() ->
    {ok, DecRec, _, <<>>} = pcep_codec:decode(BIN),
    ?assertEqual(REC, DecRec),
    {ok, EncBin, _} = pcep_codec:encode(DecRec),
    FlatBin = iolist_to_binary(EncBin),
    ?assertEqual(BIN, FlatBin)
end}).


%%% TEST CASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tlv_padding_test_() ->
    [
        ?_assertDecEnc(<<
            1:3,0:5,1,24:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,20:16,            % Open Object Header
                    1:3,0:5,0,0,0,                  % Open Object Body
                        42:16,0:16,                 % Fake TLV with no body
                        16:16,4:16,                 % PCE Caps TLV Header
                            0:29,0:1,0:1,0:1        % PCE Caps TLV Body
        >>, #pcep_msg_open{open = #pcep_obj_open{
            tlvs = [
                #pcep_tlv_unknown{type = 42, data = <<>>},
                #pcep_tlv_stateful_pce_cap{}
            ]
        }}),
        ?_assertDecEnc(<<
            1:3,0:5,1,28:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,24:16,            % Open Object Header
                    1:3,0:5,0,0,0,                  % Open Object Body
                        42:16,1:16,42,0,0,0,        % Fake TLV one byte + padding
                        16:16,4:16,                 % PCE Caps TLV Header
                            0:29,0:1,0:1,0:1        % PCE Caps TLV Body
        >>, #pcep_msg_open{open = #pcep_obj_open{
            tlvs = [
                #pcep_tlv_unknown{type = 42, data = <<42>>},
                #pcep_tlv_stateful_pce_cap{}
            ]
        }}),
        ?_assertDecEnc(<<
            1:3,0:5,1,32:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,28:16,            % Open Object Header
                    1:3,0:5,0,0,0,                  % Open Object Body
                        42:16,7:16,1,2,3,4,5,6,7,0, % Fake TLV one byte + padding
                        16:16,4:16,                 % PCE Caps TLV Header
                            0:29,0:1,0:1,0:1        % PCE Caps TLV Body
        >>, #pcep_msg_open{open = #pcep_obj_open{
            tlvs = [
                #pcep_tlv_unknown{type = 42, data = <<1,2,3,4,5,6,7>>},
                #pcep_tlv_stateful_pce_cap{}
            ]
        }})
    ].
