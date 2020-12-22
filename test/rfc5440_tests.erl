-module(rfc5440_tests).

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
open_message_test_() ->
    [
        % Basic Open Message
        ?_assertDecEnc(<<
            1:3,0:5,1,12:16,                        % Open Message Header
                1,1:4,0:2,0:1,0:1,8:16,             % Open Object Header
                    1:3,0:5,30,120,4                % Open Object Body
        >>, #pcep_msg_open{
            open = #pcep_obj_open{
                flag_p = false,
                flag_i = false,
                keepalive = 30,
                dead_timer = 120,
                sid = 4
            }
        })
    ].

% Keepalive message:    https://tools.ietf.org/html/rfc5440#section-6.3
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
keepalive_message_test_() ->
    [
        % Basic Keepalive Message
        ?_assertDecEnc(<<1:3,0:5,2,4:16>>, #pcep_msg_keepalive{})
    ].

% Computation request:  https://tools.ietf.org/html/rfc5440#section-6.4
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% RP object:            https://tools.ietf.org/html/rfc5440#section-7.4.1
% Endpoint object:      https://tools.ietf.org/html/rfc5440#section-7.6
% Bandwidth object:     https://tools.ietf.org/html/rfc5440#section-7.7
% Metric object:        https://tools.ietf.org/html/rfc5440#section-7.8
% RRO object:           https://tools.ietf.org/html/rfc5440#section-7.10
%                       https://tools.ietf.org/html/rfc3209#section-4.3
%                       https://tools.ietf.org/html/rfc3473#section-5
%                       https://tools.ietf.org/html/rfc3477#section-5
% LSPA object:          https://tools.ietf.org/html/rfc5440#section-7.11
% IRO object:           https://tools.ietf.org/html/rfc5440#section-7.12
% Load balancing obj:   https://tools.ietf.org/html/rfc5440#section-7.16
%TODO: Add support for SVEC
computation_request_message_test_() ->
    [
        % Basic IPv4 Computation request Message
        ?_assertDecEnc(<<
            1:3,0:5,3,28:16,                        % CompReq Message Header
                2,1:4,0:2,1:1,0:1,12:16,            % RP Object Header
                    0:26,1:1,1:1,0:1,3:3,           % RP Object Body
                    12345:32,
                4,1:4,0:2,1:1,0:1,12:16,            % Endpoint Object Header
                    10,1,1,1,10,1,1,2               % Endpoint Object Body
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{
                        flag_p = true,
                        flag_i = false,
                        flag_o = true,
                        flag_b = true,
                        flag_r = false,
                        pri = 3,
                        req_id = 12345,
                        tlvs = []
                    },
                    endpoints = [
                        #pcep_endpoint{
                            endpoint = #pcep_obj_endpoint_ipv4_addr{
                                flag_p = true,
                                flag_i = false,
                                source = {10, 1, 1, 1},
                                destination = {10, 1, 1, 2}
                            }   
                        }
                    ]
                }
            ]
        }),
        % Basic IPv6 Computation request Message
        ?_assertDecEnc(<<
            1:3,0:5,3,52:16,                        % CompReq Message Header
                2,1:4,0:2,1:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,1:1,0:3,           % RP Object Body
                    4294967295:32,
                4,2:4,0:2,0:1,1:1,36:16,            % Endpoint Object Header
                    16#fc00:16,0:16,0:16,0:16,      % Endpoint Object Body
                    0:16,0:16,0:16,1:16,
                    16#fc00:16,0:16,0:16,0:16,
                    0:16,0:16,0:16,2:16
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{
                        flag_p = true,
                        flag_i = false,
                        flag_o = false,
                        flag_b = false,
                        flag_r = true,
                        pri = 0,
                        req_id = 4294967295,
                        tlvs = []
                    },
                    endpoints = [
                        #pcep_endpoint{
                            endpoint = #pcep_obj_endpoint_ipv6_addr{
                                flag_p = false,
                                flag_i = true,
                                source = {16#fc00, 0, 0, 0, 0, 0, 0, 1},
                                destination = {16#fc00, 0, 0, 0, 0, 0, 0, 2}
                            }   
                        }
                    ]
                }
            ]
        }),
        % Multiple computation requests Message
        ?_assertDecEnc(<<
            1:3,0:5,3,52:16,                        % CompReq Message Header
                                                    % First computation request
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,1:3,           % RP Object Body
                    1:32,
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    10,1,1,1,10,1,1,2,              % Endpoint Object Body
                                                    % Second computation request
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,2:3,           % RP Object Body
                    2:32,
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    10,1,1,1,10,1,1,3               % Endpoint Object Body
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{pri = 1, req_id = 1},
                    endpoints = [
                        #pcep_endpoint{
                            endpoint = #pcep_obj_endpoint_ipv4_addr{
                                source = {10, 1, 1, 1},
                                destination = {10, 1, 1, 2}
                            }   
                        }
                    ]
                },
                #pcep_compreq{
                    rp = #pcep_obj_rp{pri = 2, req_id = 2},
                    endpoints = [
                        #pcep_endpoint{
                            endpoint = #pcep_obj_endpoint_ipv4_addr{
                                source = {10, 1, 1, 1},
                                destination = {10, 1, 1, 3}
                            }   
                        }
                    ]
                }
            ]
        }),
        % Computation request with LSPA and single metric
        ?_assertDecEnc(<<
            1:3,0:5,3,60:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    0,0,0,0,0,0,0,0,                % Endpoint Object Body
                9,1:4,0:2,1:1,0:1,20:16,            % LSPA Object Header
                    16#FFFF0000:32,16#FF0000FF:32,  % LSPA Object Body
                    16#F000000F:32,1,2,0:7,0:1,0,
                6,1:4,0:2,1:1,0:1,12:16,            % Metric Object Header
                    0:16,0:6,0:1,1:1,1,             % Metric Object Body
                    1200.0:32/float
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{},
                    endpoints = [#pcep_endpoint{endpoint = #pcep_obj_endpoint_ipv4_addr{}}],
                    lspa = #pcep_obj_lspa{
                        flag_p = true,
                        flag_i = false,
                        flag_l =false,
                        exclude_any = 16#FFFF0000,
                        include_any = 16#FF0000FF,
                        include_all = 16#F000000F,
                        setup_prio = 1,
                        holding_prio = 2,
                        tlvs = []
                    },
                    metrics = [
                        #pcep_obj_metric{
                            flag_p = true,
                            flag_i = false,
                            flag_c = false,
                            flag_b = true,
                            type = igp,
                            value = 1200.0
                        }
                    ]
                }
            ]
        }),
        % Computation request with Bandwidth and multiple metrics
        ?_assertDecEnc(<<
            1:3,0:5,3,60:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    0,0,0,0,0,0,0,0,                % Endpoint Object Body
                5,1:4,0:2,1:1,0:1,8:16,             % Bandwidth Object Header
                    1000.0:32/float,                % Bandwidth Object Body
                6,1:4,0:2,1:1,0:1,12:16,            % Metric 1 Object Header
                    0:16,0:6,0:1,1:1,2,             % Metric 1 Object Body
                    100.0:32/float,
                6,1:4,0:2,0:1,0:1,12:16,            % Metric 2 Object Header
                    0:16,0:6,1:1,0:1,3,             % Metric 2 Object Body
                    10.0:32/float
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{},
                    endpoints = [#pcep_endpoint{endpoint = #pcep_obj_endpoint_ipv4_addr{}}],
                    bandwidth = #pcep_obj_bandwidth_req{
                        flag_p = true,
                        flag_i = false,
                        bandwidth = 1000.0
                    },
                    metrics = [
                        #pcep_obj_metric{
                            flag_p = true,
                            flag_i = false,
                            flag_c = false,
                            flag_b = true,
                            type = te,
                            value = 100.0
                        },
                        #pcep_obj_metric{
                            flag_p = false,
                            flag_i = false,
                            flag_c = true,
                            flag_b = false,
                            type = hop_count,
                            value = 10.0
                        }
                    ]
                }
            ]
        }),
        % Computation request with RRO and bandwidth
        ?_assertDecEnc(<<
            1:3,0:5,3,72:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    0,0,0,0,0,0,0,0,                % Endpoint Object Body
                8,1:4,0:2,1:1,0:1,36:16,            % RRO Object Header
                    0:1,1:7,8,10,1,1,0,8,0,         % RRO IPv4 Sub-Object
                    1:1,2:7,20,16#fc00:16,          % RRO IPv6 Sub-Object
                        0:112,112,0,
                    0:1,32:7,4,65535:16,            % RRO ASN Sub-Object
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    123.0:32/float                  % Bandwidth Object Body
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{pri = 0, req_id = 0},
                    endpoints = [#pcep_endpoint{
                            endpoint = #pcep_obj_endpoint_ipv4_addr{},
                            paths = [
                                #pcep_rpath{
                                    rro = #pcep_obj_rro{
                                        flag_p = true,
                                        flag_i = false,
                                        path = [
                                            #pcep_ro_subobj_ipv4{
                                                flag_l = false,
                                                prefix = {{10, 1, 1, 0}, 8}
                                            },
                                            #pcep_ro_subobj_ipv6{
                                                flag_l = true,
                                                prefix = {{16#fc00, 0, 0, 0, 0, 0, 0, 0}, 112}
                                            },
                                            #pcep_ro_subobj_asn{
                                                flag_l = false,
                                                asn = 65535
                                            }
                                        ]
                                    },
                                    bandwidth = #pcep_obj_bandwidth_req{
                                        bandwidth = 123.0
                                    }
                                }
                            ]
                        }
                    ]
                }
            ]
        }),
        % Computation request with LSPA, RRO, IRO, dual bandwidth and load-balancing
        ?_assertDecEnc(<<
            1:3,0:5,3,84:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                4,1:4,0:2,0:1,0:1,12:16,            % Endpoint Object Header
                    0,0,0,0,0,0,0,0,                % Endpoint Object Body
                9,1:4,0:2,0:1,0:1,20:16,            % LSPA Object Header
                    0:32,0:32,0:32,0,0,0:7,0:1,0,   % LSPA Object Body
                5,2:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    0.0:32/float,                   % Bandwidth Object Body
                8,1:4,0:2,0:1,0:1,4:16,             % RRO Object Header
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    0.0:32/float,                   % Bandwidth Object Body
                10,1:4,0:2,0:1,0:1,4:16,            % IRO Object Header
                14,1:4,0:2,0:1,0:1,12:16,           % Load-Balancing Obj. Header
                    0:16,0,126,1000.0:32/float      % Load-Balancing Obj. Body
        >>, #pcep_msg_compreq{
            request_list = [
                #pcep_compreq{
                    rp = #pcep_obj_rp{},
                    endpoints = [#pcep_endpoint{endpoint = #pcep_obj_endpoint_ipv4_addr{}}],
                    lspa = #pcep_obj_lspa{},
                    bandwidth = #pcep_obj_bandwidth_telsp{},
                    rpath = #pcep_rpath{
                        rro = #pcep_obj_rro{},
                        bandwidth = #pcep_obj_bandwidth_req{}
                    },
                    iro = #pcep_obj_iro{},
                    load_balancing = #pcep_obj_load_balancing{
                        max_lsp =  126,
                        min_bandwidth = 1000.0
                    }
                }
            ]
        })
    ].

% Computation reply:    https://tools.ietf.org/html/rfc5440#section-6.5
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% RP object:            https://tools.ietf.org/html/rfc5440#section-7.4.1
% NO-PATH object:       https://tools.ietf.org/html/rfc5440#section-7.5
% Endpoint object:      https://tools.ietf.org/html/rfc5440#section-7.6
% Bandwidth object:     https://tools.ietf.org/html/rfc5440#section-7.7
% Metric object:        https://tools.ietf.org/html/rfc5440#section-7.8
% ERO object:           https://tools.ietf.org/html/rfc5440#section-7.9
% LSPA object:          https://tools.ietf.org/html/rfc5440#section-7.11
% IRO object:           https://tools.ietf.org/html/rfc5440#section-7.12
computation_reply_message_test_() ->
    [
        % Basic computation reply message
        ?_assertDecEnc(<<
            1:3,0:5,4,16:16,                        % CompReq Message Header
                2,1:4,0:2,1:1,0:1,12:16,            % RP Object Header
                    0:26,1:1,1:1,0:1,3:3,           % RP Object Body
                    12345:32
        >>, #pcep_msg_comprep{
            reply_list = [
                #pcep_comprep{
                    rp = #pcep_obj_rp{
                        flag_p = true,
                        flag_i = false,
                        flag_o = true,
                        flag_b = true,
                        flag_r = false,
                        pri = 3,
                        req_id = 12345,
                        tlvs = []
                    }
                }
            ]
        }),
        % Computation reply message with ERO, bandwidth and metrics
        ?_assertDecEnc(<<
            1:3,0:5,4,56:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    0.0:32/float,                   % Bandwidth Object Body
                6,1:4,0:2,0:1,0:1,12:16,            % Metric Object Header
                    0:16,0:6,0:1,0:1,1,             % Metric Object Body
                    0.0:32/float,
                7,1:4,0:2,0:1,0:1,20:16,            % ERO Object Header
                    0:1,1:7,8,0,0,0,0,0,0,          % ERO IPv4 Sub-Object
                    0:1,1:7,8,0,0,0,0,0,0           % ERO IPv4 Sub-Object

        >>, #pcep_msg_comprep{
            reply_list = [
                #pcep_comprep{
                    rp = #pcep_obj_rp{},
                    bandwidth = #pcep_obj_bandwidth_req{},
                    metrics = [#pcep_obj_metric{}],
                    endpoints = [
                        #pcep_endpoint{
                            paths = [
                                #pcep_path{
                                    ero = #pcep_obj_ero{
                                        path = [
                                            #pcep_ro_subobj_ipv4{},
                                            #pcep_ro_subobj_ipv4{}
                                        ]
                                    }
                                }
                            ]
                        }
                    ]
                }
            ]
        }),
        % Computation reply message with multiple ERO
        ?_assertDecEnc(<<
            1:3,0:5,4,40:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                7,1:4,0:2,0:1,0:1,12:16,            % ERO Object Header
                    0:1,1:7,8,10,1,1,0,8,0,         % RRO IPv4 Sub-Object
                7,1:4,0:2,0:1,0:1,12:16,            % ERO Object Header
                    0:1,1:7,8,10,1,2,0,8,0          % RRO IPv4 Sub-Object
        >>, #pcep_msg_comprep{
            reply_list = [
                #pcep_comprep{
                    rp = #pcep_obj_rp{},
                    endpoints = [
                        #pcep_endpoint{
                            paths = [
                                #pcep_path{
                                    ero = #pcep_obj_ero{
                                        path = [
                                            #pcep_ro_subobj_ipv4{
                                                prefix = {{10, 1, 1, 0}, 8}
                                            }
                                        ]
                                    }
                                },
                                #pcep_path{
                                    ero = #pcep_obj_ero{
                                        path = [
                                            #pcep_ro_subobj_ipv4{
                                                prefix = {{10, 1, 2, 0}, 8}
                                            }
                                        ]
                                    }
                                }
                            ]
                        }
                    ]
                }
            ]
        }),
        % Computation reply message with bandwidth, no path and TLV
        ?_assertDecEnc(<<
            1:3,0:5,4,40:16,                        % CompReq Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,0:32,      % RP Object Body
                3,1:4,0:2,0:1,1:1,16:16,            % NO-PATH Object Header
                    0,1:1,0:15,0,                   % NO-PATH Object Body
                        1:16,4:16,0:29,1:1,0:1,1:1, % NO-PATH-VECTOR TLV
                5,1:4,0:2,0:1,0:1,8:16,             % Bandwidth Object Header
                    0.0:32/float                    % Bandwidth Object Body
        >>, #pcep_msg_comprep{
            reply_list = [
                #pcep_comprep{
                    rp = #pcep_obj_rp{},
                    nopath = #pcep_obj_nopath{
                        flag_p = false,
                        flag_i = true,
                        flag_c = true,
                        ni = path_not_found,
                        tlvs = [
                            #pcep_tlv_nopath_vector{
                                pce_not_available = true,
                                unknown_destination = false,
                                unknown_source = true
                            }
                        ]
                    } ,
                    bandwidth = #pcep_obj_bandwidth_req{}
                }
            ]
        })
    ].

% Notification message: https://tools.ietf.org/html/rfc5440#section-6.6
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% RP object:            https://tools.ietf.org/html/rfc5440#section-7.4.1
% Notif object:         https://tools.ietf.org/html/rfc5440#section-7.14
notification_message_test_() ->
    [
        % Basic notification message
        ?_assertDecEnc(<<
            1:3,0:5,5,12:16,                        % Notif Message Header
                12,1:4,0:2,0:1,0:1,8:16,            % Notif Object Header
                    0,0,2,2                         % Notif Object Body
        >>, #pcep_msg_notif{
            notif_list = [
                #pcep_notif{
                    id_list = [],
                    notifications = [
                        #pcep_obj_notif{
                            type = pce_congestion,
                            value = pce_recovered
                        }
                    ]
                }
            ]
        }),
        % Complex message with multiple instances of everything
        ?_assertDecEnc(<<
            1:3,0:5,5,64:16,                        % Notif Message Header
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,1:32,      % RP Object Body
                12,1:4,0:2,0:1,0:1,8:16,            % Notif Object Header
                    0,0,1,1,                        % Notif Object Body
                12,1:4,0:2,0:1,0:1,8:16,            % Notif Object Header
                    0,0,1,2,                        % Notif Object Body
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,2:32,      % RP Object Body
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,3:32,      % RP Object Body
                12,1:4,0:2,0:1,0:1,8:16,            % Notif Object Header
                    0,0,2,1                         % Notif Object Body
        >>, #pcep_msg_notif{
            notif_list = [
                #pcep_notif{
                    id_list = [
                        #pcep_obj_rp{req_id = 1}
                    ],
                    notifications = [
                        #pcep_obj_notif{
                            type = req_canceled,
                            value = pcc_canceled_req
                        },
                        #pcep_obj_notif{
                            type = req_canceled,
                            value = pce_canceled_req
                        }
                    ]
                },
                #pcep_notif{
                    id_list = [
                        #pcep_obj_rp{req_id = 2},
                        #pcep_obj_rp{req_id = 3}
                    ],
                    notifications = [
                        #pcep_obj_notif{
                            type = pce_congestion,
                            value = pce_congested
                        }
                    ]
                }
            ]
        })
    ].

% Error message:        https://tools.ietf.org/html/rfc5440#section-6.7
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% RP object:            https://tools.ietf.org/html/rfc5440#section-7.4.1
% Error object:         https://tools.ietf.org/html/rfc5440#section-7.15
error_message_test_() ->
    [
        % Basic error message
        ?_assertDecEnc(<<
            1:3,0:5,6,12:16,                        % Error Message Header
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,3,2                         % Error Object Body
        >>, #pcep_msg_error{
            error_list = [
                #pcep_error{
                    id_list = [],
                    errors = [
                        #pcep_obj_error{
                            type = unknown_object,
                            value = unrec_object_type
                        }
                    ]
                }
            ]
        }),
        % Complex error message
        ?_assertDecEnc(<<
            1:3,0:5,6,88:16,                        % Error Message Header
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,1,1,                        % Error Object Body
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,1,2,                        % Error Object Body
                1,1:4,0:2,0:1,0:1,8:16,             % Open Object Header
                    1:3,0:5,0,0,0,                  % Open Object Body
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,1:32,      % RP Object Body
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,2:32,      % RP Object Body
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,6,1,                        % Error Object Body
                2,1:4,0:2,0:1,0:1,12:16,            % RP Object Header
                    0:26,0:1,0:1,0:1,0:3,3:32,      % RP Object Body
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,7,0,                        % Error Object Body
                13,1:4,0:2,0:1,0:1,8:16,            % Error Object Header
                    0,0,8,0                         % Error Object Body
        >>, #pcep_msg_error{
            error_list = [
                #pcep_error{
                    id_list = [],
                    errors = [
                        #pcep_obj_error{
                            type = session_failure,
                            value = invalid_open_message
                        },
                        #pcep_obj_error{
                            type = session_failure,
                            value = openwait_timed_out
                        }
                    ],
                    open = #pcep_obj_open{}
                },
                #pcep_error{
                    id_list = [
                        #pcep_obj_rp{req_id = 1},
                        #pcep_obj_rp{req_id = 2}
                    ],
                    errors = [
                        #pcep_obj_error{
                            type = mandatory_object_missing,
                            value = rp_object_missing
                        }
                    ]
                },
                #pcep_error{
                    id_list = [
                        #pcep_obj_rp{req_id = 3}
                    ],
                    errors = [
                        #pcep_obj_error{
                            type = sync_pc_req_missing,
                            value = sync_pc_req_missing
                        },
                        #pcep_obj_error{
                            type = unknown_req_ref,
                            value = unknown_req_ref
                        }
                    ]
                }
            ]
        })
    ].

% Close message:        https://tools.ietf.org/html/rfc5440#section-6.8
% Message header:       https://tools.ietf.org/html/rfc5440#section-6.1
% Object header:        https://tools.ietf.org/html/rfc5440#section-7.2
% Close object:         https://tools.ietf.org/html/rfc5440#section-7.17
close_message_test_() ->
    [
        % Basic close message
        ?_assertDecEnc(<<
            1:3,0:5,7,12:16,                        % Close Message Header
                15,1:4,0:2,0:1,0:1,8:16,            % Close Object Header
                    0,0,0,2                         % Close Object Body
        >>, #pcep_msg_close{close = #pcep_obj_close{reason = deadtimer}})
    ].
