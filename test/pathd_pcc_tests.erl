-module(pathd_pcc_tests).

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

open_message_test_() ->
    [
        %TODO: Implemente the deprecated SR-PCE-CAP for pathd compatibility with draft07
        ?_assertDecEnc(<<
            1:3,0:5,1,40:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,36:16,            % Open Object Header
                    1:3,0:5,30,120,4,               % Open Object Body
                    16:16,4:16,                     % Stateful PCE Caps TLV Header
                        0:29,0:1,0:1,1:1,           % Stateful PCE Caps TLV Body
                    26:16,4:16,                     % SR PCE CAP TLV Header
                        0,0,0,32,                   % SR PCE CAP TLV Body
                    34:16,8:16,                     % PST CAP TLV Header
                        0,0,0,1,1,0,0,0             % PST CAP TLV Body
        >>, #pcep_msg_open{open = #pcep_obj_open{
            flag_p = false,
            flag_i = false,
            keepalive = 30,
            dead_timer = 120,
            sid = 4,
            tlvs = [
                #pcep_tlv_stateful_pce_cap{flag_u = true},
                #pcep_tlv_sr_pce_cap{msd = 32},
                #pcep_tlv_path_setup_type_cap{psts=[srte]}
            ]
        }})
    ].

report_message_test_() ->
    [
        % Synchronization report
        ?_assertDecEnc(<<
            1:3,0:5,10,112:16,                      % Report Message Header
                33,1:4,0:2,1:1,0:1,20:16,           % SRP Object Header
                    0:32,0:32,                      % SRP Object Body
                    28:16,4:16,                     % PST TLV Header
                        0,0,0,1,                    % PST TLV Body
                32,1:4,0:2,1:1,0:1,52:16,           % LSP Object Header
                    1:20,0:5,4:3,0:1,0:1,1:1,0:1,   % LSP Object Body
                        18:16,16:16,                % IPv4 LSP Identifier TLV
                            10,10,10,10,0:16,
                            0:16,10,10,10,10,
                            10,10,10,2,
                        17:16,8:16,"two-cp22",      % LSP Symbolic Name TLV
                        65505:16,6:16,              % Unknown TLV Header
                            0,0,0,138,224,0,0,0,    % Unknown TLV Body + Padding
                7,1:4,0:2,1:1,0:1,36:16,            % ERO Object Header
                    0:1,36:7,16,                    % ERO SR Sub-Object header
                        3:4,0:8,0:1,0:1,0:1,1:1,    % ERO SR Sub-Object Header
                        0:20,0:3,0:1,0:8,           % ERO SR Sub-object SID
                        10,1,2,11,10,1,2,1,         % IPv4 Adjacency NAI
                    0:1,36:7,16,                    % ERO SR Sub-Object header
                        3:4,0:8,0:1,0:1,0:1,1:1,    % ERO SR Sub-Object Header
                        0:20,0:3,0:1,0:8,           % ERO SR Sub-object SID
                        10,1,20,1,10,1,20,2         % IPv4 Adjacency NAI
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    srp = #pcep_obj_srp{
                        flag_p = true,
                        tlvs = [#pcep_tlv_path_setup_type{pst = srte}]
                    },
                    lsp = #pcep_obj_lsp{
                        flag_p = true,
                        flag_s = true,
                        plsp_id = 1,
                        status = going_up,
                        tlvs = [
                            #pcep_tlv_ipv4_lsp_id{
                                source = {10, 10, 10, 10},
                                lsp_id = 0,
                                tunnel_id = 0,
                                extended_id = <<10, 10, 10, 10>>,
                                endpoint = {10, 10, 10, 2}
                            },
                            #pcep_tlv_symbolic_path_name{
                                name = <<"two-cp22">>
                            },
                            #pcep_tlv_unknown{
                                type = 65505,
                                data = <<0,0,0,138,224,0>>
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{
                        flag_p = true,
                        path = [
                            #pcep_ro_subobj_sr{
                                has_sid = true,
                                has_nai = true,
                                nai_type = ipv4_adjacency,
                                is_mpls = true,
                                sid = #mpls_stack_entry{label = 0},
                                nai = #sr_nai_adjacency{
                                    local_address = {10,1,2,11},
                                    remote_address = {10,1,2,1}
                                }
                            },
                            #pcep_ro_subobj_sr{
                                has_sid = true,
                                has_nai = true,
                                nai_type = ipv4_adjacency,
                                is_mpls = true,
                                sid = #mpls_stack_entry{label = 0},
                                nai = #sr_nai_adjacency{
                                    local_address = {10,1,20,1},
                                    remote_address = {10,1,20,2}
                                }
                            }
                        ]
                    }
                }
            ]
        }),
        % End-Of-Sync Report Message
        ?_assertDecEnc(<<
            1:3,0:5,10,36:16,                       % Report Message Header
                32,1:4,0:2,1:1,0:1,28:16,           % LSP Object Header
                    0:20,0:5,0:3,0:1,0:1,0:1,0:1,   % LSP Object Body
                        18:16,16:16,                % IPv4 LSP Identifier TLV
                            0,0,0,0,0:16,
                            0:16,0,0,0,0,
                            0,0,0,0,
                7,1:4,0:2,1:1,0:1,4:16             % ERO Object Header
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{
                        flag_p = true,
                        flag_s = false,
                        plsp_id = 0,
                        status = down,
                        tlvs = [#pcep_tlv_ipv4_lsp_id{}]
                    },
                    ero = #pcep_obj_ero{flag_p = true}
                }
            ]
        }),
        % State Report Message
        ?_assertDecEnc(<<
            1:3,0:5,10,128:16,                      % Report Message Header
                33,1:4,0:2,1:1,0:1,20:16,           % SRP Object Header
                    0:32,0:32,                      % SRP Object Body
                    28:16,4:16,                     % PST TLV Header
                        0,0,0,1,                    % PST TLV Body
                32,1:4,0:2,1:1,0:1,52:16,           % LSP Object Header
                    5:20,0:4,1:1,2:3,               % LSP Object Body
                    1:1,0:1,0:1,1:1,
                        18:16,16:16,                % IPv4 LSP Identifier TLV
                            10,10,10,10,0:16,
                            0:16,10,10,10,10,
                            10,10,10,1,
                        17:16,8:16,"one-cp11",      % LSP Symbolic Name TLV
                        65505:16,6:16,              % Unknown TLV Header
                            0,0,0,69,112,0,0,0,     % Unknown TLV Body + Padding
                7,1:4,0:2,1:1,0:1,20:16,            % ERO Object Header
                    0:1,36:7,16,                    % ERO SR Sub-Object header
                        3:4,0:8,0:1,0:1,0:1,1:1,    % ERO SR Sub-Object Body
                        17001:20,0:3,0:1,0:8,       % ERO SR Sub-object SID
                        10,1,2,11,10,1,2,1,         % IPv4 Adjacency NAI
                9,1:4,0:2,1:1,0:1,20:16,            % LSPA Object Header
                    1:32,0:32,0:32,4,4,0:7,0:1,0,   % LSPA Object Body
                6,1:4,0:2,0:1,0:1,12:16,            % Metric Object Header
                    0:16,0:6,0:1,0:1,2,0.0:32/float % Metric Object Body
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    srp = #pcep_obj_srp{
                        flag_p = true,
                        tlvs = [#pcep_tlv_path_setup_type{pst = srte}]
                    },
                    lsp = #pcep_obj_lsp{
                        flag_p = true,
                        flag_a = true,
                        flag_d = true,
                        flag_c = true,
                        plsp_id = 5,
                        status = active,
                        tlvs = [
                            #pcep_tlv_ipv4_lsp_id{
                                source = {10, 10, 10, 10},
                                lsp_id = 0,
                                tunnel_id = 0,
                                extended_id = <<10, 10, 10, 10>>,
                                endpoint = {10, 10, 10, 1}
                            },
                            #pcep_tlv_symbolic_path_name{
                                name = <<"one-cp11">>
                            },
                            #pcep_tlv_unknown{
                                type = 65505,
                                data = <<0,0,0,69,112,0>>
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{
                        flag_p = true,
                        path = [
                            #pcep_ro_subobj_sr{
                                has_sid = true,
                                has_nai = true,
                                nai_type = ipv4_adjacency,
                                is_mpls = true,
                                sid = #mpls_stack_entry{label = 17001},
                                nai = #sr_nai_adjacency{
                                    local_address = {10,1,2,11},
                                    remote_address = {10,1,2,1}
                                }
                            }
                        ]
                    },
                    lspa = #pcep_obj_lspa{
                        flag_p = true,
                        exclude_any = 1,
                        setup_prio = 4,
                        holding_prio = 4
                    },
                    metrics = [
                        #pcep_obj_metric{
                            type = te,
                            value = 0.0
                        }
                    ]
                }
            ]
        })
    ].

computation_request_test_() ->
    [
        ?_assertDecEnc(<<
            1:3,0:5,3,56:16,                        % CompReq Message Header
                2,1:4,0:2,1:1,0:1,20:16,            % RP Object Header
                    0:24,1:1,0:1,0:1,0:1,0:1,       % RP Object Body
                    0:3,1:32,
                    28:16,4:16,                     % Path Setup Type TLV Header
                        0,0,0,1,                    % Path Setup Type TLV Body
                4,1:4,0:2,1:1,0:1,12:16,            % Endpoint Object Header
                    10,10,10,10,10,10,10,1,         % Endpoint Object Body
                9,1:4,0:2,1:1,0:1,20:16,            % LSPA Object Header
                    16#00000001:32,16#00000000:32,  % LSPA Object Body
                    16#00000000:32,4,4,0:7,0:1,0
    >>, #pcep_msg_compreq{
        request_list = [
            #pcep_compreq{
                rp = #pcep_obj_rp{
                    flag_p = true,
                    flag_s = true,
                    req_id = 1,
                    tlvs = [
                        #pcep_tlv_path_setup_type{pst = srte}
                    ]
                },
                endpoints = [
                    #pcep_endpoint{
                        endpoint = #pcep_obj_endpoint_ipv4_addr{
                            flag_p = true,
                            source = {10,10,10,10},
                            destination = {10,10,10,1}
                        }
                    }
                ],
                lspa = #pcep_obj_lspa{
                    flag_p = true,
                    exclude_any = 16#00000001,
                    include_any = 16#00000000,
                    include_all = 16#00000000,
                    setup_prio = 4,
                    holding_prio = 4
                }
            }
        ]})
    ].
