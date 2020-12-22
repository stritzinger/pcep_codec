-module(pcep_decode_tests).

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


%%% TEST CASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decoding_error_test_() ->
    %TODO: rp object's capability_not_supported
    %TODO: metric object's capability_not_supported
    %TODO: ero object's capability_not_supported when the sub-object type is unknown
    %TODO: ipv4 subobj's malformed_ro_subobj when the prefix size is > 32
    %TODO: ipv6 subobj's malformed_ro_subobj when the prefix size is > 128
    %TODO: notif object's unknown_notif_type when the notif type is unknown
    %TODO: notif object's unknown_notif_value when the notif type is unknown
    %TODO: close object's unknown_close_reason when the reason type is unknown
    %TODO: lsp_error_code TLV's unknown_lsp_error when the error code is unknown
    [
        ?_assertMatch({error, pcep_version_not_supported,
                       #{type := pcep_version_not_supported}, [], _},
                      pcep_codec:decode(<<64,1,0,12,1,16,0,8,32,30,120,4>>)),
        ?_assertMatch({error, unknown_message,
                       #{type := unknown_message}, [], _},
                      pcep_codec:decode(<<32,43,0,8,1,16,0,4>>)),
        ?_assertMatch({error, {mandatory_error,{codec,open}},
                       #{type := missing_object, object_class := open}, [], _},
                      pcep_codec:decode(<<32,1,0,4>>)),
        ?_assertMatch({error, unknown_object_class,
                       #{type := unknown_object_class}, [], _},
                      pcep_codec:decode(<<32,1,0,16,1,16,0,8,32,30,120,4,42,1:4,0:2,1:1,0:1,0,4>>)),
        ?_assertMatch({error, unknown_object_type,
                       #{type := unknown_object_type}, [], _},
                      pcep_codec:decode(<<32,1,0,16,1,16,0,8,32,30,120,4,1,16:4,0:2,1:1,0:1,0,4>>)),
        ?_assertMatch({error, malformed_object,
                       #{type := malformed_object}, [], _},
                      pcep_codec:decode(<<32,1,0,10,1,16,0,6,32,30>>)),
        ?_assertMatch({error, malformed_tlv,
                       #{type := malformed_tlv}, [], _},
                      pcep_codec:decode(<<32,1,0,19,1,16,0,15,32,30,120,4,0,16,0,3,0,0,0>>)),
        ?_assertMatch({error, malformed_tlv,
                       #{type := malformed_tlv}, [], _},
                      pcep_codec:decode(<<32,1,0,16,1,16,0,12,32,30,120,4,0,16,0,4>>))
    ].

decoding_warning_test_() ->
    %TODO: rp object's capability_not_supported
    %TODO: metric object's capability_not_supported
    [
        ?_assertMatch({ok, #pcep_msg_open{}, [#{type := unknown_object_class}], _},
                      pcep_codec:decode(<<
            32,1,0,16,                  % Open Message Header
                42,1:4,0:2,0:1,0:1,0,4, % Fake Unknown Object Header
                1,16,0,8,               % Open Object Header
                    32,30,120,4         % Open Object Body
        >>)),
        ?_assertMatch({ok, #pcep_msg_open{}, [#{type := unknown_object_type}], _},
                      pcep_codec:decode(<<32,1,0,16,1,16,0,8,32,30,120,4,1,16:4,0:2,0:1,0:1,0,4>>)),
        ?_assertMatch({ok, #pcep_msg_open{}, [#{type := unknown_tlv_type}], _},
                      pcep_codec:decode(<<32,1,0,16,1,16,0,12,32,30,120,4,0,42,0,0>>))
    ].

decode_incomplete_test_() ->
    [
        ?_assertMatch({more, undefined}, pcep_codec:decode(<<>>)),
        ?_assertMatch({more, undefined}, pcep_codec:decode(<<32,1>>)),
        ?_assertMatch({more, 12}, pcep_codec:decode(<<32,1,0,12,1>>))
    ].
