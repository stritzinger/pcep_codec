-module(cisco_pce_tests).

-include_lib("eunit/include/eunit.hrl").

-include("pcep_codec.hrl").

% From Cisco IOS XR Software, Version 6.5.1

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

-define(_assertDec(BIN, REC), {?LINE, fun() ->
    {ok, DecRec, _, <<>>} = pcep_codec:decode(BIN),
    ?assertEqual(REC, DecRec)
end}).

-define(_assertDecEnc(BIN, REC), {?LINE, fun() ->
    {ok, DecRec, _, <<>>} = pcep_codec:decode(BIN),
    ?assertEqual(REC, DecRec),
    {ok, EncBin, _} = pcep_codec:encode(DecRec),
    FlatBin = iolist_to_binary(EncBin),
    ?assertEqual(BIN, FlatBin)
end}).


%%% TEST CASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_message_test_() ->
    [
        ?_assertDecEnc(<<
            1:3,0:5,1,28:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,24:16,            % Open Object Header
                    1:3,0:5,60,120,0,               % Open Object Body
                    16:16,4:16,                     % PCE Caps TLV Header
                        0:29,1:1,0:1,1:1,           % PCE Caps TLV Body
                    26:16,4:16,                     % SR PCE CAP TLV Header
                        0,0,0,10                    % SR PCE CAP TLV Body
        >>, #pcep_msg_open{open = #pcep_obj_open{
            flag_p = false,
            flag_i = false,
            keepalive = 60,
            dead_timer = 120,
            sid = 0,
            tlvs = [
                #pcep_tlv_stateful_pce_cap{flag_u = true, flag_i = true},
                #pcep_tlv_sr_pce_cap{msd = 10}
            ]
        }})
    ].

computation_reply_test_() ->
    [
        ?_assertDecEnc(<<
            1:3,0:5,4,56:16,                        % CompRep Message Header
                2,1:4,0:2,1:1,0:1,20:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,1:32,      % RP Object Body
                    28:16,4:16,                     % Path Setup Type TLV Header
                        0,0,0,1,                    % Path Setup Type TLV Body
                7,1:4,0:2,0:1,0:1,20:16,            % ERO Object Header
                    0:1,36:7,16,                    % ERO SR Sub-Object header
                        3:4,0:8,0:1,0:1,0:1,1:1,    % ERO SR Sub-Object Header
                        17001:20,0:3,0:1,0:8,       % ERO SR Sub-object SID
                        10,1,2,11,10,1,2,1,         % IPv4 Adjacency NAI
                6,1:4,0:2,0:1,0:1,12:16,            % Metric Object Header
                    0:16,0:6,0:1,0:1,2,0.0:32/float % Metric Object Body
    >>, #pcep_msg_comprep{
        reply_list = [
            #pcep_comprep{
                rp = #pcep_obj_rp{
                    flag_p = true,
                    flag_i = false,
                    flag_o = false,
                    flag_b = false,
                    flag_r = false,
                    pri = 0,
                    req_id = 1,
                    tlvs = [
                        #pcep_tlv_path_setup_type{pst = srte}
                    ]
                },
                endpoints = [
                    #pcep_endpoint{
                        paths = [
                            #pcep_path{
                                ero = #pcep_obj_ero{
                                    flag_p = false,
                                    flag_i = false,
                                    path = [
                                        #pcep_ro_subobj_sr{
                                            flag_l = false,
                                            has_sid = true,
                                            has_nai = true,
                                            nai_type = ipv4_adjacency,
                                            is_mpls = true,
                                            has_mpls_extra = false,
                                            sid = #mpls_stack_entry{
                                                label = 17001
                                            },
                                            nai = #sr_nai_adjacency{
                                                local_address = {10,1,2,11},
                                                remote_address = {10,1,2,1}
                                            }
                                        }
                                    ]
                                },
                                metrics = [
                                    #pcep_obj_metric{
                                        type = te,
                                        value = 0.0
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]})
    ].


error_message_test_() ->
    [
        ?_assertDecEnc(<<
            1:3,0:5,6,12:16,                        % CompRep Message Header
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,6,9                         % Error Object Body
        >>, #pcep_msg_error{
            error_list = [
                #pcep_error{
                    id_list = [],
                    errors = [
                        #pcep_obj_error{
                            type = mandatory_object_missing,
                            value = ero_object_missing
                        }
                    ]
                }
            ]
        })
    ].
