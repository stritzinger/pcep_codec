-module(rfc8231_tests).

-include_lib("eunit/include/eunit.hrl").

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").


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

%  0                   1                   2                   3
%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
% Common Message Header:
% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
% | Ver |  Flags  |  Message-Type |       Message-Length          |
% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
% Common Object Header:
% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
% | Object-Class  |   OT  |Res|P|I|   Object Length (bytes)       |
% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


% Open message:         https://tools.ietf.org/html/rfc5440#section-6.2
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% Open object:          https://tools.ietf.org/html/rfc5440#section-7.3
% Stateful Cap TLV:     https://tools.ietf.org/html/rfc8231#section-7.1.1
open_message_test_() ->
    [
        % Open Message with PCE stateful capabilities TLV
        ?_assertDecEnc(<<
            1:3,0:5,1,20:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,16:16,            % Open Object Header
                    1:3,0:5,0,0,0,                  % Open Object Body
                        16:16,4:16,0:31,1:1         % pcep_codec Stateful Cap TLV
        >>, #pcep_msg_open{
            open = #pcep_obj_open{
                tlvs = [
                    #pcep_tlv_stateful_pce_cap{flag_u = true}
                ]
            }
        })
    ].

% Report message:       https://tools.ietf.org/html/rfc8231#section-6.1
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% Bandwidth object:     https://tools.ietf.org/html/rfc5440#section-7.7
% Metric object:        https://tools.ietf.org/html/rfc5440#section-7.8
% ERO object:           https://tools.ietf.org/html/rfc5440#section-7.9
% LSPA object:          https://tools.ietf.org/html/rfc5440#section-7.11
% IRO object:           https://tools.ietf.org/html/rfc5440#section-7.12
% SRP object:           https://tools.ietf.org/html/rfc8231#section-7.2
% LSP object:           https://tools.ietf.org/html/rfc8231#section-7.3
% LSP ID TLV:           https://tools.ietf.org/html/rfc8231#section-7.3.1
% Symbolic name TLV:    https://tools.ietf.org/html/rfc8231#section-7.3.2
%TODO: Add support for RSVP SPEC TLV: https://tools.ietf.org/html/rfc8231#section-7.3.4
report_message_test_() ->
    [
        % Basic report message
        ?_assertDecEnc(<<
                1:3,0:5,10,32:16,                   % Report Message Header
                32,1:4,0:2,0:1,1:1,8:16,            % LSP Object Header
                    986892:20,0:5,                  % LSP Object Body
                    4:3,1:1,0:1,1:1,0:1,
                7,1:4,0:2,0:1,0:1,20:16,            % ERO Object Header
                    0:1,1:7,8,0,0,0,0,0,0,          % ERO IPv4 Sub-Object
                    0:1,1:7,8,0,0,0,0,0,0           % ERO IPv4 Sub-Object
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{
                        flag_p = false,
                        flag_i = true,
                        flag_d = false,
                        flag_s = true,
                        flag_r = false,
                        flag_a = true,
                        plsp_id = 986892,
                        status = going_up
                    },
                    ero = #pcep_obj_ero{
                        path = [
                            #pcep_ro_subobj_ipv4{},
                            #pcep_ro_subobj_ipv4{}
                        ]
                    }
                }
            ]
        }),
        % Report message with IPv4 LSP ID TLV
        ?_assertDecEnc(<<
                1:3,0:5,10,36:16,                   % Report Message Header
                32,1:4,0:2,0:1,0:1,28:16,           % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                        18:16,16:16,                % IPv4 LSP Identifier TLV
                        10,1,1,2,60000:16,
                        40000:16,1,2,3,4,
                        10,1,1,10,
                7,1:4,0:2,0:1,0:1,4:16              % ERO Object Header
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{
                        tlvs = [
                            #pcep_tlv_ipv4_lsp_id{
                                source = {10, 1, 1, 2},
                                lsp_id = 60000,
                                tunnel_id = 40000,
                                extended_id = <<1, 2, 3, 4>>,
                                endpoint = {10, 1, 1, 10}
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{}
                }
            ]
        }),
        % Report message with IPv6 LSP ID TLV
        ?_assertDecEnc(<<
                1:3,0:5,10,72:16,                   % Report Message Header
                32,1:4,0:2,0:1,0:1,64:16,           % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                        19:16,52:16,                % IPv6 LSP Identifier TLV
                        16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,1:16,
                        1:16,2:16,
                        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
                        16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,2:16,
                7,1:4,0:2,0:1,0:1,4:16              % ERO Object Header
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{
                        tlvs = [
                            #pcep_tlv_ipv6_lsp_id{
                                source = {16#fc00, 0, 0, 0, 0, 0, 0, 1},
                                lsp_id = 1,
                                tunnel_id = 2,
                                extended_id = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                                11, 12, 13, 14, 15, 16>>,
                                endpoint = {16#fc00, 0, 0, 0, 0, 0, 0, 2}
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{}
                }
            ]
        }),
        % Report message with LSP symbolic name TLV (not multiple of 4)
        ?_assertDecEnc(<<
                1:3,0:5,10,24:16,                   % Report Message Header
                32,1:4,0:2,0:1,0:1,16:16,           % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                        17:16,3:16,"foo",0,         % LSP Symbolic Name TLV
                7,1:4,0:2,0:1,0:1,4:16              % ERO Object Header
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{
                        tlvs = [
                            #pcep_tlv_symbolic_path_name{
                                name = <<"foo">>
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{}
                }
            ]
        }),
        % Report message with LSP symbolic name TLV (multiple of 4)
        ?_assertDecEnc(<<
                1:3,0:5,10,28:16,                   % Report Message Header
                32,1:4,0:2,0:1,0:1,20:16,           % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                        17:16,8:16,"<foobar>",      % IPv4 LSP Identifier TLV
                7,1:4,0:2,0:1,0:1,4:16              % ERO Object Header
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{
                        tlvs = [
                            #pcep_tlv_symbolic_path_name{
                                name = <<"<foobar>">>
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{}
                }
            ]
        }),
        % Multiple report message
        ?_assertDecEnc(<<
                1:3,0:5,10,64:16,                   % Report Message Header
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    1:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                7,1:4,0:2,0:1,0:1,12:16,            % ERO Object Header
                    0:1,1:7,8,10,1,1,0,8,0,         % ERO IPv4 Sub-Object
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    2:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                7,1:4,0:2,0:1,0:1,12:16,            % ERO Object Header
                    0:1,1:7,8,10,2,1,0,8,0,         % ERO IPv4 Sub-Object
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    3:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                7,1:4,0:2,0:1,0:1,12:16,            % ERO Object Header
                    0:1,1:7,8,10,3,1,0,8,0          % ERO IPv4 Sub-Object
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{plsp_id = 1},
                    ero = #pcep_obj_ero{
                        path = [#pcep_ro_subobj_ipv4{
                            prefix = {{10, 1, 1, 0}, 8}}
                        ]
                    }
                },
                #pcep_report{
                    lsp = #pcep_obj_lsp{plsp_id = 2},
                    ero = #pcep_obj_ero{
                        path = [#pcep_ro_subobj_ipv4{
                            prefix = {{10, 2, 1, 0}, 8}}
                        ]
                    }
                },
                #pcep_report{
                    lsp = #pcep_obj_lsp{plsp_id = 3},
                    ero = #pcep_obj_ero{
                        path = [#pcep_ro_subobj_ipv4{
                            prefix = {{10, 3, 1, 0}, 8}}
                        ]
                    }
                }
            ]
        }),
        % Report message with SRP, bandwidth and metrics
        ?_assertDecEnc(<<
                1:3,0:5,10,68:16,                   % Report Message Header
                33,1:4,0:2,1:1,0:1,12:16,           % SRP Object Header
                    0:32,4042322124:32,
                32,1:4,0:2,1:1,0:1,8:16,            % LSP Object Header
                    12:20,0:5,                      % LSP Object Body
                    3:3,0:1,0:1,0:1,0:1,
                7,1:4,0:2,0:1,0:1,12:16,            % ERO Object Header
                    0:1,1:7,8,0,0,0,0,0,0,          % ERO IPv4 Sub-Object
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    0.0:32/float,                   % Bandwidth Object Body
                6,1:4,0:2,0:1,0:1,12:16,            % Metric 1 Object Header
                    0:16,0:6,0:1,0:1,1,             % Metric 1 Object Body
                    0.0:32/float,
                6,1:4,0:2,0:1,0:1,12:16,            % Metric 2 Object Header
                    0:16,0:6,0:1,0:1,2,             % Metric 2 Object Body
                    0.0:32/float
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    srp = #pcep_obj_srp{
                        flag_p = true,
                        srp_id = 4042322124
                    },
                    lsp = #pcep_obj_lsp{
                        flag_p = true,
                        plsp_id = 12,
                        status = going_down
                    },
                    ero = #pcep_obj_ero{
                        path = [#pcep_ro_subobj_ipv4{}]
                    },
                    bandwidth = #pcep_obj_bandwidth_req{},
                    metrics = [
                        #pcep_obj_metric{type = igp},
                        #pcep_obj_metric{type = te}
                    ]
                }
            ]
        }),
        % Report message with actual and intended attributes
        ?_assertDecEnc(<<
                1:3,0:5,10,84:16,                   % Report Message Header
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                7,1:4,0:2,0:1,0:1,4:16,             % ERO Object Header
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    1.0:32/float,                   % Bandwidth Object Body
                6,1:4,0:2,0:1,0:1,12:16,            % Metric Object Header
                    0:16,0:6,0:1,0:1,1,             % Metric Object Body
                    1.0:32/float,
                8,1:4,0:2,0:1,0:1,4:16,             % RRO Object Header
                9,1:4,0:2,0:1,0:1,20:16,            % LSPA Object Header
                    0:32,0:32,0:32,0,0,0:7,0:1,0,   % LSPA Object Body
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    2.0:32/float,                   % Bandwidth Object Body
                6,1:4,0:2,0:1,0:1,12:16,            % Metric Object Header
                    0:16,0:6,0:1,0:1,1,             % Metric Object Body
                    2.0:32/float,
                10,1:4,0:2,0:1,0:1,4:16             % IRO Object Header
        >>, #pcep_msg_report{
            report_list = [
                #pcep_report{
                    lsp = #pcep_obj_lsp{},
                    ero = #pcep_obj_ero{},
                    rro = #pcep_obj_rro{},
                    actual_bandwidth = #pcep_obj_bandwidth_req{bandwidth = 1.0},
                    actual_metrics = [#pcep_obj_metric{value = 1.0}],
                    lspa = #pcep_obj_lspa{},
                    bandwidth = #pcep_obj_bandwidth_req{bandwidth = 2.0},
                    metrics = [#pcep_obj_metric{value = 2.0}],
                    iro = #pcep_obj_iro{}
                }
            ]
        })
    ].

% Update message:       https://tools.ietf.org/html/rfc8231#section-6.2
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% Bandwidth object:     https://tools.ietf.org/html/rfc5440#section-7.7
% Metric object:        https://tools.ietf.org/html/rfc5440#section-7.8
% ERO object:           https://tools.ietf.org/html/rfc5440#section-7.9
% LSPA object:          https://tools.ietf.org/html/rfc5440#section-7.11
% IRO object:           https://tools.ietf.org/html/rfc5440#section-7.12
% SRP object:           https://tools.ietf.org/html/rfc8231#section-7.2
% LSP object:           https://tools.ietf.org/html/rfc8231#section-7.3
% LSP error code TLV:   https://tools.ietf.org/html/rfc8231#section-7.3.3
update_message_test_() ->
    [
        % Basic update message
        ?_assertDecEnc(<<
                1:3,0:5,11,44:16,                   % Update Message Header
                33,1:4,0:2,0:1,0:1,12:16,           % SRP Object Header
                    0:32,0:32,
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                7,1:4,0:2,0:1,0:1,20:16,            % ERO Object Header
                    0:1,1:7,8,0,0,0,0,0,0,          % ERO IPv4 Sub-Object
                    0:1,1:7,8,0,0,0,0,0,0           % ERO IPv4 Sub-Object
        >>, #pcep_msg_update{
            update_list = [
                #pcep_update{
                    srp = #pcep_obj_srp{},
                    lsp = #pcep_obj_lsp{},
                    ero = #pcep_obj_ero{
                        path = [
                            #pcep_ro_subobj_ipv4{},
                            #pcep_ro_subobj_ipv4{}
                        ]
                    }
                }
            ]
        }),
        % Update message with an LSP
        ?_assertDecEnc(<<
                1:3,0:5,11,52:16,                   % Update Message Header
                33,1:4,0:2,0:1,0:1,12:16,           % SRP Object Header
                    0:32,0:32,
                32,1:4,0:2,0:1,0:1,16:16,           % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                        20:16,4:16,4:32,            % LSP Error Code TLV
                7,1:4,0:2,0:1,0:1,20:16,            % ERO Object Header
                    0:1,1:7,8,0,0,0,0,0,0,          % ERO IPv4 Sub-Object
                    0:1,1:7,8,0,0,0,0,0,0           % ERO IPv4 Sub-Object
        >>, #pcep_msg_update{
            update_list = [
                #pcep_update{
                    srp = #pcep_obj_srp{},
                    lsp = #pcep_obj_lsp{
                        tlvs = [
                            #pcep_tlv_lsp_error_code{
                                error = unacceptable_parameters
                            }
                        ]
                    },
                    ero = #pcep_obj_ero{
                        path = [
                            #pcep_ro_subobj_ipv4{},
                            #pcep_ro_subobj_ipv4{}
                        ]
                    }
                }
            ]
        })
    ].

% Error message:        https://tools.ietf.org/html/rfc5440#section-6.7
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% Error object:         https://tools.ietf.org/html/rfc5440#section-7.15
% SRP object:           https://tools.ietf.org/html/rfc8231#section-7.2
error_message_test_() ->
    [
        % Error message with SRP
        ?_assertDecEnc(<<
            1:3,0:5,6,44:16,                        % Error Message Header
                33,1:4,0:2,0:1,0:1,12:16,           % SRP Object Header
                    0:32,1:32,
                33,1:4,0:2,0:1,0:1,12:16,           % SRP Object Header
                    0:32,2:32,
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,1,1,                        % Error Object Body
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,1,2                         % Error Object Body
        >>, #pcep_msg_error{
            error_list = [
                #pcep_error{
                    id_list = [
                        #pcep_obj_srp{srp_id = 1},
                        #pcep_obj_srp{srp_id = 2}
                    ],
                    errors = [
                        #pcep_obj_error{
                            type = session_failure,
                            value = invalid_open_message
                        },
                        #pcep_obj_error{
                            type = session_failure,
                            value = openwait_timed_out
                        }
                    ]
                }
            ]
        })
    ].

% Computation request:  https://tools.ietf.org/html/rfc5440#section-6.4
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% RP object:            https://tools.ietf.org/html/rfc5440#section-7.4.1
% Endpoint object:      https://tools.ietf.org/html/rfc5440#section-7.6
% LSP object:           https://tools.ietf.org/html/rfc8231#section-7.3
computation_request_message_test_() ->
    [
        % Computation request message with LSP
        ?_assertDecEnc(<<
            1:3,0:5,3,36:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    10,1,1,1,10,1,1,2,              % Endpoint Object Body
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{},
                    endpoints = [
                        #pcep_endpoint{
                            endpoint = #pcep_obj_endpoint_ipv4_addr{
                                source = {10, 1, 1, 1},
                                destination = {10, 1, 1, 2}
                            }   
                        }
                    ],
                    lsp = #pcep_obj_lsp{}
                }
            ]
        })
    ].

% Computation reply:    https://tools.ietf.org/html/rfc5440#section-6.5
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% RP object:            https://tools.ietf.org/html/rfc5440#section-7.4.1
% NO-PATH object:       https://tools.ietf.org/html/rfc5440#section-7.5
% LSP object:           https://tools.ietf.org/html/rfc8231#section-7.3
computation_reply_message_test_() ->
    [
        % Computation reply no path and LSP
        ?_assertDecEnc(<<
            1:3,0:5,4,32:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                32,1:4,0:2,0:1,0:1,8:16,            % LSP Object Header
                    0:20,0:5,                       % LSP Object Body
                    0:3,0:1,0:1,0:1,0:1,
                3,1:4,0:2,0:1,0:1,8:16,            % NO-PATH Object Header
                    0,0:1,0:15,0                    % NO-PATH Object Body
        >>, #pcep_msg_comprep{
            reply_list = [
                #pcep_comprep{
                    rp = #pcep_obj_rp{},
                    nopath = #pcep_obj_nopath{},
                    lsp = #pcep_obj_lsp{}
                }
            ]
        })
    ].

