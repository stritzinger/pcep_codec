-module(pcep_codec_object).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([decode/2]).
-export([decode_spec/3]).
-export([encode/2]).
-export([encode_spec/3]).


%%% IMPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-import(pcep_codec_utils, [bit2bool/1]).
-import(pcep_codec_utils, [bool2bit/1]).
-import(pcep_codec_utils, [addr4/1, addr6/1]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Ctx, Data) ->
    decode_all(Ctx, [], Data).

decode_spec(Ctx, Spec, Data) ->
    codec_sequencer:decode(Spec, Data, #{
        context => Ctx,
        decoder => fun decode_class/3,
        reset_error => fun pcep_codec_context:reset_error/1,
        mandatory_handler => fun pcep_codec_error:missing_mandatory_object/2
    }).


encode(Ctx, Obj) ->
    encode_all(Ctx, 0, [], Obj).

encode_spec(Ctx, Spec, Obj) ->
    codec_sequencer:encode(Spec, Obj, #{
        context => Ctx,
        encoder => fun encode_class/3,
        reset_error => fun pcep_codec_context:reset_error/1,
        mandatory_handler => fun pcep_codec_error:missing_mandatory_object/2
    }).


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-- Decoding Functions ---------------------------------------------------------

decode_all(Ctx, Acc, Data) ->
    case decode_any(Ctx, Data) of
        {not_found, Ctx2} ->
            {ok, lists:reverse(Acc), <<>>, Ctx2};
        {ok, Obj, RemData, Ctx2} ->
            decode_all(Ctx2, [Obj | Acc], RemData);
        {error, _Reason, _Ctx2} = Result ->
            Result
    end.

decode_any(Ctx, <<>>) ->
    {not_found, Ctx};
decode_any(Ctx, Data) ->
    case decode_header(Ctx, Data) of
        {ignore, RemData, Ctx2} ->
            decode_any(Ctx2, RemData);
        {ok, Obj, _Class, Type, Body, RemData, Ctx2} ->
            case decode_object(Ctx2, Type, Obj, Body) of
                {ok, Obj2, Ctx3} ->
                    {ok, Obj2, RemData, pcep_codec_context:commit_object(Ctx3)};
                {ignore, Ctx3} ->
                    decode_any(Ctx3, RemData);
                {error, _Reason, _Ctx3} = Result ->
                    Result
            end;
        {error, _Reason, _Ctx2} = Result ->
            Result
    end.

decode_class(Ctx, _Class, <<>>) ->
    ?DEBUG("DECODING ~w (empty)", [_Class]),
    {not_found, Ctx};
decode_class(Ctx, Class, Data) ->
    ?DEBUG("DECODING ~w", [Class]),
    case decode_header(Ctx, Data) of
        {ignore, RemData, Ctx2} ->
            decode_class(Ctx2, Class, RemData);
        {ok, Header, Class, Type, Body, RemData, Ctx2} ->
            case decode_object(Ctx2, Type, Header, Body) of
                {ok, Obj2, Ctx3} ->
                    {ok, Obj2, RemData, pcep_codec_context:commit_object(Ctx3)};
                {ignore, Ctx3} ->
                    decode_class(Ctx3, Class, RemData);
                {error, Reason, Ctx3} ->
                    Ctx4 = pcep_codec_context:update_error(Ctx3,
                                #{object_class => Class}),
                    {error, Reason, Ctx4}
            end;
        {ok, _Header, _OtherClass, _Type, _Body, _RemData, Ctx2} ->
            ?DEBUG("    ~w NOT FOUND", [Class]),
            {not_found, Ctx2};
        {error, Reason, Ctx2} ->
            Ctx3 = pcep_codec_context:update_error(Ctx2,
                        #{object_class => Class}),
            {error, Reason, Ctx3}
    end.

% https://tools.ietf.org/html/rfc5440#section-7.2
decode_header(Ctx, <<C:8, T:4, _:2, P:1, I:1, L:16,
                     Body:(L-4)/binary, Rem/binary>> = Data) ->
    <<ObjData:L/binary, _/binary>> = Data,
    Ctx2 = Ctx#{object_data => ObjData},
    case {P, pcep_codec_iana:int_object_class_type(C, T)} of
        {1, {undefined, undefined}} ->
            Ctx3 = pcep_codec_context:ignore_object(Ctx2),
            pcep_codec_error:unknown_object_class(Ctx3, C, T);
        {1, {Class, undefined}} ->
            Ctx3 = pcep_codec_context:ignore_object(Ctx2),
            pcep_codec_error:unknown_object_type(Ctx3, Class, C, T);
        {0, {undefined, undefined}} ->
            Ctx3 = pcep_codec_context:ignore_object(Ctx2),
            {ignore, Rem,
             pcep_codec_warning:unknown_object_class(Ctx3, C, T)};
        {0, {Class, undefined}} ->
            Ctx3 = pcep_codec_context:ignore_object(Ctx2),
            {ignore, Rem,
             pcep_codec_warning:unknown_object_type(Ctx3, Class, C, T)};
        {_, {Class, Type}} ->
            Ctx3 = pcep_codec_context:begin_object(Ctx2, Class, Type),
            Header = #{flag_p => bit2bool(P), flag_i => bit2bool(I)},
            {ok, Header, Class, Type, Body, Rem, Ctx3}
    end;
decode_header(Ctx, Data) ->
    Ctx2 = Ctx#{object_data => Data},
    pcep_codec_error:malformed_object(Ctx2).

% https://tools.ietf.org/html/rfc5440#section-7.3
decode_object(Ctx, open, Header,
        <<?PCEP_VER:3, _:5, Keepalive:8, DeadTimer:8, SID:8, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    decode_tlvs(Ctx, Rem, #pcep_obj_open.tlvs, #pcep_obj_open{
        flag_p = P,
        flag_i = I,
        keepalive = Keepalive,
        dead_timer = DeadTimer,
        sid = SID
    });
decode_object(Ctx, open, _Header, <<?PCEP_VER:3, _:5, _/binary>>) ->
    pcep_codec_error:malformed_object(Ctx);
decode_object(Ctx, open, _Header, <<V:3, _:5, _/binary>>) ->
    pcep_codec_error:version_not_supported(Ctx, V);
% https://tools.ietf.org/html/rfc5440#section-7.4
% https://tools.ietf.org/html/rfc5541#section-3.3
decode_object(Ctx, rp, Header,
              <<_:24, S:1, _:1, O:1, B:1, R:1, Pri:3, ReqId:32,
                Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    decode_tlvs(Ctx, Rem, #pcep_obj_rp.tlvs, #pcep_obj_rp{
        flag_p = P,
        flag_i = I,
        flag_o = bit2bool(O),
        flag_b = bit2bool(B),
        flag_r = bit2bool(R),
        flag_s = bit2bool(S),
        pri = Pri,
        req_id = ReqId
    });
% https://tools.ietf.org/html/rfc5440#section-7.5
decode_object(Ctx, nopath, Header, <<NI:8, C:1, _:15, _:8, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    case {P, pcep_codec_iana:int_nopath_nature(NI)} of
        {true, undefined} ->
            pcep_codec_error:capability_not_supported(Ctx,
                "unknown nopath object's nature ~w", [NI]);
        {false, undefined} ->
            {ignore, pcep_codec_warning:capability_not_supported(Ctx,
                        "unknown nopath object's nature ~w", [NI])};
        {_, Nature} ->
            decode_tlvs(Ctx, Rem, #pcep_obj_nopath.tlvs, #pcep_obj_nopath{
                flag_p = P,
                flag_i = I,
                flag_c = bit2bool(C),
                ni = Nature
            })
    end;
% https://tools.ietf.org/html/rfc5440#section-7.6
decode_object(Ctx, endpoint_ipv4_addr, Header,
              <<S1:8,S2:8,S3:8,S4:8,D1:8,D2:8,D3:8,D4:8>>) ->
    #{flag_p := P, flag_i := I} = Header,
    {ok, #pcep_obj_endpoint_ipv4_addr{
        flag_p = P,
        flag_i = I,
        source = {S1, S2, S3, S4},
        destination = {D1, D2, D3, D4}
    }, Ctx};
decode_object(Ctx, endpoint_ipv6_addr, Header,
              <<S1:16,S2:16,S3:16,S4:16,S5:16,S6:16,S7:16,S8:16,
                D1:16,D2:16,D3:16,D4:16,D5:16,D6:16,D7:16,D8:16>>) ->
    #{flag_p := P, flag_i := I} = Header,
    {ok, #pcep_obj_endpoint_ipv6_addr{
        flag_p = P,
        flag_i = I,
        source = {S1, S2, S3, S4, S5, S6, S7, S8},
        destination = {D1, D2, D3, D4, D5, D6, D7, D8}
    }, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.7
decode_object(Ctx, bandwidth_req, Header, <<B:32/float>>) ->
    #{flag_p := P, flag_i := I} = Header,
    {ok, #pcep_obj_bandwidth_req{
        flag_p = P,
        flag_i = I,
        bandwidth = B
    }, Ctx};
decode_object(Ctx, bandwidth_telsp, Header, Data) ->
    % We reuse the decoding of bandwidth_req and just change the record name.
    decode_alt_object(Ctx, bandwidth_req, pcep_obj_bandwidth_telsp, Header, Data);
decode_object(Ctx, bandwidth_cisco, Header, Data) ->
    % We reuse the decoding of bandwidth_req and just change the record name.
    decode_alt_object(Ctx, bandwidth_req, pcep_obj_bandwidth_cisco, Header, Data);
% https://tools.ietf.org/html/rfc5440#section-7.8
decode_object(Ctx, metric, Header, <<_:16, _:6, C:1, B:1, T:8, V:32/float>>) ->
    #{flag_p := P, flag_i := I} = Header,
    case {P, pcep_codec_iana:int_metric_type(T)} of
        {true, undefined} ->
            pcep_codec_error:capability_not_supported(Ctx,
                "unknown metric type ~w", [T]);
        {false, undefined} ->
            {ignore, pcep_codec_warning:capability_not_supported(Ctx,
                        "unknown metric type ~w", [T])};
        {_, Type} ->
            {ok, #pcep_obj_metric{
                flag_p = P,
                flag_i = I,
                flag_c = bit2bool(C),
                flag_b = bit2bool(B),
                type = Type,
                value = V
            }, Ctx}
    end;
% https://tools.ietf.org/html/rfc5440#section-7.9
% https://tools.ietf.org/html/rfc3209#section-4.3.2
decode_object(Ctx, ero, Header, Data) ->
    #{flag_p := P, flag_i := I} = Header,
    case decode_ro_subobjs(Ctx, Data) of
        {ok, SubObjs, Ctx2} ->
            {ok, #pcep_obj_ero{
                flag_p = P,
                flag_i = I,
                path = SubObjs
            }, Ctx2};
        {error, _Reason, _Ctx2} = Result ->
            Result
    end;
% https://tools.ietf.org/html/rfc5440#section-7.10
% https://tools.ietf.org/html/rfc3209#section-4.3.2
decode_object(Ctx, rro, Header, Data) ->
    % We reuse the decoding of ero and just change the record name.
    decode_alt_object(Ctx, ero, pcep_obj_rro, Header, Data);
% https://tools.ietf.org/html/rfc5440#section-7.12
% https://tools.ietf.org/html/rfc3209#section-4.3.2
decode_object(Ctx, iro, Header, Data) ->
    % We reuse the decoding of ero and just change the record name.
    decode_alt_object(Ctx, ero, pcep_obj_iro, Header, Data);
% https://tools.ietf.org/html/rfc5440#section-7.11
decode_object(Ctx, lspa, Header,
              <<ExcAny:32, IncAny:32, IncAll:32, SetupPrio:8, HoldPrio:8,
                _:7, L:1, _:8, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    decode_tlvs(Ctx, Rem, #pcep_obj_lspa.tlvs, #pcep_obj_lspa{
        flag_p = P,
        flag_i = I,
        flag_l = bit2bool(L),
        exclude_any = ExcAny,
        include_any = IncAny,
        include_all = IncAll,
        setup_prio = SetupPrio,
        holding_prio = HoldPrio
    });
% TODO: SVEC https://tools.ietf.org/html/rfc5440#section-7.13.2
% https://tools.ietf.org/html/rfc5440#section-7.14
decode_object(Ctx, notif, Header, <<_:16, T:8, V:8, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    case pcep_codec_iana:int_notif(T, V) of
        {undefined, _} ->
            pcep_codec_error:unknown_notification_type(Ctx, T, V);
        {Type, undefined} ->
            pcep_codec_error:unknown_notification_value(Ctx, Type, T, V);
        {Type, Value} ->
            decode_tlvs(Ctx, Rem, #pcep_obj_notif.tlvs, #pcep_obj_notif{
                flag_p = P,
                flag_i = I,
                type = Type,
                value = Value
            })
    end;
% https://tools.ietf.org/html/rfc5440#section-7.15
decode_object(Ctx, error, Header, <<_:16, T:8, V:8, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    case pcep_codec_iana:int_error(T, V) of
        {undefined, _} ->
            pcep_codec_error:unknown_error_type(Ctx, T, V);
        {Type, undefined} ->
            pcep_codec_error:unknown_error_value(Ctx, Type, T, V);
        {Type, Value} ->
            decode_tlvs(Ctx, Rem, #pcep_obj_error.tlvs, #pcep_obj_error{
                flag_p = P,
                flag_i = I,
                type = Type,
                value = Value
            })
    end;
% https://tools.ietf.org/html/rfc5440#section-7.16
decode_object(Ctx, load_balancing, Header, <<_:16, _:8, M:8, B:32/float>>) ->
    #{flag_p := P, flag_i := I} = Header,
    {ok, #pcep_obj_load_balancing{
        flag_p = P,
        flag_i = I,
        max_lsp = M,
        min_bandwidth = B
    }, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.17
decode_object(Ctx, close, Header, <<_:16, _:8, R:8, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    case pcep_codec_iana:int_close_reason(R) of
        undefined ->
            pcep_codec_error:unknown_close_reason(Ctx, R);
        Reason ->
            decode_tlvs(Ctx, Rem, #pcep_obj_close.tlvs, #pcep_obj_close{
                flag_p = P,
                flag_i = I,
                reason = Reason
            })
    end;
% https://tools.ietf.org/html/rfc8231#section-7.2
decode_object(Ctx, srp, Header, <<_:32, ID:32, Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    decode_tlvs(Ctx, Rem, #pcep_obj_srp.tlvs, #pcep_obj_srp{
        flag_p = P,
        flag_i = I,
        srp_id = ID
    });
% https://tools.ietf.org/html/rfc8231#section-7.3
% https://tools.ietf.org/html/rfc8281#section-8.2
decode_object(Ctx, lsp, Header, <<PLSPID:20, _:4, C:1, O:3, A:1, R:1, S:1, D:1,
              Rem/binary>>) ->
    #{flag_p := P, flag_i := I} = Header,
    case pcep_codec_iana:int_opstatus(O) of
        undefined ->
            pcep_codec_error:unknown_lsp_opstatus(Ctx, O);
        OpStatus ->
            decode_tlvs(Ctx, Rem, #pcep_obj_lsp.tlvs, #pcep_obj_lsp{
                flag_p = P,
                flag_i = I,
                flag_d = bit2bool(D),
                flag_s = bit2bool(S),
                flag_r = bit2bool(R),
                flag_a = bit2bool(A),
                flag_c = bit2bool(C),
                plsp_id = PLSPID,
                status = OpStatus
            })
    end;
decode_object(Ctx, _Type, _Obj, _Data) ->
    pcep_codec_error:malformed_object(Ctx).

decode_alt_object(Ctx, Type, AltType, Obj, Data) ->
    case decode_object(Ctx, Type, Obj, Data) of
        {ok, Rec, Ctx2} -> {ok, setelement(1, Rec, AltType), Ctx2};
        Other -> Other
    end.

decode_tlvs(Ctx, Rem, Idx, Rec) ->
    case pcep_codec_tlv:decode(Ctx, Rem) of
        {ok, TLVs, Ctx2} ->
            {ok, setelement(Idx, Rec, TLVs), Ctx2};
        {error, _Reason, _Ctx2} = Result ->
            Result
    end.

% https://tools.ietf.org/html/rfc3209#section-4.3.2
decode_ro_subobjs(Ctx, Data) ->
    decode_ro_subobjs(Ctx, [], Data).

decode_ro_subobjs(Ctx, Acc, <<>>) ->
    {ok, lists:reverse(Acc), Ctx};
decode_ro_subobjs(Ctx, Acc, <<L:1, T:7, Len:8, B:(Len-2)/binary,
                              Rem/binary>> = Data) ->
    <<SubObjData:L/binary, _/binary>> = Data,
    Ctx2 = Ctx#{subobj_data => SubObjData},
    case pcep_codec_iana:int_ro_subobj_type(T) of
        undefined ->
            pcep_codec_error:capability_not_supported(Ctx2,
                "unknown RO sub-object type ~w", [T]);
        Type ->
            Ctx3 = pcep_codec_context:begin_subobj(Ctx2, Type),
            Header = #{flag_l => bit2bool(L)},
            case decode_ro_subobj(Ctx3, Type, Header, B) of
                {ok, SubObj, Ctx4} ->
                    decode_ro_subobjs(Ctx4, [SubObj | Acc], Rem);
                {error, _Reason, _Ctx4} = Result ->
                    Result
            end
    end.

% https://tools.ietf.org/html/rfc3209#section-4.3.3.2
decode_ro_subobj(Ctx, ipv4, Header, <<A1:8, A2:8, A3:8, A4:8, PS:8, _:8>>) ->
    #{flag_l := L} = Header,
    case PS =< 32 of
        false ->
            pcep_codec_error:malformed_ro_subobj(Ctx,
                "invalid IPv4 prefix size ~w", [PS]);
        true ->
            {ok, #pcep_ro_subobj_ipv4{
                flag_l = L,
                prefix = {{A1, A2, A3, A4}, PS}
            }, Ctx}
    end;
% https://tools.ietf.org/html/rfc3209#section-4.3.3.3
decode_ro_subobj(Ctx, ipv6, Header, <<A1:16, A2:16, A3:16, A4:16,
                                      A5:16, A6:16, A7:16, A8:16,
                                      PS:8, _:8>>) ->
    #{flag_l := L} = Header,
    case PS =< 128 of
        false ->
            pcep_codec_error:malformed_ro_subobj(Ctx,
                "invalid IPv6 prefix size ~w", [PS]);
        true ->
            {ok, #pcep_ro_subobj_ipv6{
                flag_l = L,
                prefix = {{A1, A2, A3, A4, A5, A6, A7, A8}, PS}
            }, Ctx}
    end;
% https://tools.ietf.org/html/rfc3477#section-4
decode_ro_subobj(Ctx, unum, Header, <<_:16, R1:8, R2:8, R3:8, R4:8, I:32>>) ->
    #{flag_l := L} = Header,
    {ok, #pcep_ro_subobj_unum{
        flag_l = L,
        router_id = {R1, R2, R3, R4},
        interface_id = I
    }, Ctx};
% https://tools.ietf.org/html/rfc3209#section-4.3.3.4
decode_ro_subobj(Ctx, asn, Header, <<ASN:16>>) ->
    #{flag_l := L} = Header,
    {ok, #pcep_ro_subobj_asn{
        flag_l = L,
        asn = ASN
    }, Ctx};
% https://tools.ietf.org/html/rfc8664#section-4.3.1
decode_ro_subobj(Ctx, sr, Header,
                 <<NT:4, _:8, F:1, S:1, C:1, M:1, Rem/binary>>) ->
    #{flag_l := L} = Header,
    case pcep_codec_iana:int_nai_type(NT) of
        undefined ->
            pcep_codec_error:unknown_sr_nai_type(Ctx, NT);
        NAIType ->
            Rec = #pcep_ro_subobj_sr{flag_l = L},
            case decode_sr_ro_sid(Ctx, Rec, S, M, C, Rem) of
                {ok, Rec2, Rem2, Ctx2} ->
                    case decode_sr_ro_nai(Ctx2, Rec2, NAIType, F, Rem2) of
                        {ok, Rec3, <<>>, Ctx3} ->
                            {ok, Rec3, Ctx3};
                        {ok, _Rec3, _, Ctx3} ->
                            pcep_codec_error:malformed_ro_subobj(Ctx3,
                                "invalid SR subobject, unexpected extra data", []);
                        {error, _Reason, _Ctx3} = Result ->
                            Result
                    end;
                {error, _Reason, _Ctx2} = Result ->
                    Result
            end

    end;
decode_ro_subobj(Ctx, _Type, _Header, _Data) ->
    pcep_codec_error:malformed_object(Ctx).


decode_sr_ro_sid(Ctx, Rec, 1, _, _, Rem) ->
    {ok, Rec, Rem, Ctx};
decode_sr_ro_sid(Ctx, Rec, 0, 0, _, <<SID:32, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{has_sid = true, sid = SID}, Rem, Ctx};
decode_sr_ro_sid(Ctx, Rec, 0, 1, 0, <<L:20, _:12, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_sid = true,
        is_mpls = true,
        has_mpls_extra = false,
        sid = #mpls_stack_entry{label = L}
    }, Rem, Ctx};
decode_sr_ro_sid(Ctx, Rec, 0, 1, 1,
                 <<L:20, TC:3, S:1, TTL:8, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_sid = true,
        is_mpls = true,
        has_mpls_extra = true,
        sid = #mpls_stack_entry_ext{
            label = L,
            tc = TC,
            is_last = bit2bool(S),
            ttl = TTL
        }
    }, Rem, Ctx};
decode_sr_ro_sid(Ctx, _Rec, _S, _M, _C, _Data) ->
    pcep_codec_error:malformed_ro_subobj(Ctx,
        "invalid SR subobject, malformed or missing SID", []).

decode_sr_ro_nai(Ctx, Rec, absent, 1, Rem) ->
    {ok, Rec, Rem, Ctx};
decode_sr_ro_nai(Ctx, Rec, ipv4_node = NAIType, 0,
                 <<A1:8, A2:8, A3:8, A4:8, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_nai = true,
        nai_type = NAIType,
        nai = #srte_nai_node{
            address = {A1, A2, A3, A4}
        }
    }, Rem, Ctx};
decode_sr_ro_nai(Ctx, Rec, ipv6_node = NAIType, 0,
                 <<A1:16, A2:16, A3:16, A4:16,
                   A5:16, A6:16, A7:16, A8:16, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_nai = true,
        nai_type = NAIType,
        nai = #srte_nai_node{
            address = {A1, A2, A3, A4, A5, A6, A7, A8}
        }
    }, Rem, Ctx};
decode_sr_ro_nai(Ctx, Rec, ipv4_adjacency = NAIType, 0,
                 <<L1:8, L2:8, L3:8, L4:8,
                   R1:8, R2:8, R3:8, R4:8, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_nai = true,
        nai_type = NAIType,
        nai = #srte_nai_adjacency{
            local_address = {L1, L2, L3, L4},
            remote_address = {R1, R2, R3, R4}
        }
    }, Rem, Ctx};
decode_sr_ro_nai(Ctx, Rec, ipv6_adjacency = NAIType, 0,
                 <<L1:16, L2:16, L3:16, L4:16,
                   L5:16, L6:16, L7:16, L8:16,
                   R1:16, R2:16, R3:16, R4:16,
                   R5:16, R6:16, R7:16, R8:16, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_nai = true,
        nai_type = NAIType,
        nai = #srte_nai_adjacency{
            local_address = {L1, L2, L3, L4, L5, L6, L7, L8},
            remote_address = {R1, R2, R3, R4, R5, R6, R7, R8}
        }
    }, Rem, Ctx};
decode_sr_ro_nai(Ctx, Rec, unumbered_ipv4_adjacency = NAIType, 0,
                 <<LNId:32, LIId:32, RNId: 32, RIId:32, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_nai = true,
        nai_type = NAIType,
        nai = #srte_nai_unumbered_ipv4_adjacency{
            local_node_id = LNId,
            local_interface_id = LIId,
            remote_node_id = RNId,
            remote_interface_id = RIId
        }
    }, Rem, Ctx};
decode_sr_ro_nai(Ctx, Rec, linklocal_ipv6_adjacency = NAIType, 0,
                 <<L1:16, L2:16, L3:16, L4:16,
                   L5:16, L6:16, L7:16, L8:16, LIId:32,
                   R1:16, R2:16, R3:16, R4:16,
                   R5:16, R6:16, R7:16, R8:16, RIId:32, Rem/binary>>) ->
    {ok, Rec#pcep_ro_subobj_sr{
        has_nai = true,
        nai_type = NAIType,
        nai = #srte_nai_linklocal_ipv6_adjacency{
            local_address = {L1, L2, L3, L4, L5, L6, L7, L8},
            local_interface_id = LIId,
            remote_address = {R1, R2, R3, R4, R5, R6, R7, R8},
            remote_interface_id = RIId
        }
    }, Rem, Ctx};
decode_sr_ro_nai(Ctx, _Rec, NAIType, _F, _Data) ->
    pcep_codec_error:malformed_ro_subobj(Ctx,
        "invalid SR subobject, malformed ~w NAI", [NAIType]).


%-- Encoding Functions ---------------------------------------------------------

encode_all(Ctx, Size, Acc, []) ->
    {ok, lists:reverse(Acc), Size, [], Ctx};
encode_all(Ctx, Size, Acc, [Obj | Rem]) ->
    case encode_class(Ctx, Acc, Obj) of
        {not_found, Ctx2} ->
            {ok, lists:reverse(Acc), Size, Rem, Ctx2};
        {ok, Data, Size2, Ctx2} ->
            encode_all(Ctx2, Size + Size2, [Data | Acc], Rem);
        {error, _Reason, _Ctx2} = Error ->
            Error
    end.

% https://tools.ietf.org/html/rfc5440#section-7.2
pack_object(Ctx, Class, Type, P, I, BodySize, Body, []) ->
    {C, T} = pcep_codec_iana:pcep_object_class_type(Class, Type),
    L = BodySize + 4,
    Data = [<<C:8, T:4, 0:2, (bool2bit(P)):1, (bool2bit(I)):1, L:16>>, Body],
    {ok, Data, L, pcep_codec_context:commit_object(Ctx)};
pack_object(Ctx, Class, Type, P, I, BodySize, Body, TLVs) ->
    {C, T} = pcep_codec_iana:pcep_object_class_type(Class, Type),
    case pcep_codec_tlv:encode(Ctx, TLVs) of
        {ok, TlvBin, TlvSize, Ctx2} ->
            L = BodySize + TlvSize + 4,
            Data = [<<C:8, T:4, 0:2, (bool2bit(P)):1, (bool2bit(I)):1, L:16>>,
                    Body, TlvBin],
            {ok, Data, L, pcep_codec_context:commit_object(Ctx2)};
        {error, _Reason, _Ctx2} = Error ->
            Error
    end.

object_class_type(#pcep_obj_open{}) ->
    {open, open};
object_class_type(#pcep_obj_rp{}) ->
    {rp, rp};
object_class_type(#pcep_obj_nopath{}) ->
    {nopath, nopath};
object_class_type(#pcep_obj_endpoint_ipv4_addr{}) ->
    {endpoint, endpoint_ipv4_addr};
object_class_type(#pcep_obj_endpoint_ipv6_addr{}) ->
    {endpoint, endpoint_ipv6_addr};
object_class_type(#pcep_obj_bandwidth_req{}) ->
    {bandwidth, bandwidth_req};
object_class_type(#pcep_obj_bandwidth_telsp{}) ->
    {bandwidth, bandwidth_telsp};
object_class_type(#pcep_obj_bandwidth_cisco{}) ->
    {bandwidth, bandwidth_cisco};
object_class_type(#pcep_obj_metric{}) ->
    {metric, metric};
object_class_type(#pcep_obj_ero{}) ->
    {ero, ero};
object_class_type(#pcep_obj_rro{}) ->
    {rro, rro};
object_class_type(#pcep_obj_iro{}) ->
    {iro, iro};
object_class_type(#pcep_obj_lspa{}) ->
    {lspa, lspa};
object_class_type(#pcep_obj_notif{}) ->
    {notif, notif};
object_class_type(#pcep_obj_error{}) ->
    {error, error};
object_class_type(#pcep_obj_load_balancing{}) ->
    {load_balancing, load_balancing};
object_class_type(#pcep_obj_close{}) ->
    {close, close};
object_class_type(#pcep_obj_srp{}) ->
    {srp, srp};
object_class_type(#pcep_obj_lsp{}) ->
    {lsp, lsp};
object_class_type(_Other) ->
    undefined.

encode_class(Ctx, Class, Obj) ->
    ?DEBUG("ENCODING ~w", [Class]),
    case object_class_type(Obj) of
        {Class, Type} ->
            Ctx2 = pcep_codec_context:begin_object(Ctx, Class, Type),
            case encode_object(Ctx2, Obj) of
                {ok, P, I, Data, Size, TLVs, Ctx3} ->
                    pack_object(Ctx3, Class, Type, P, I, Size, Data, TLVs);
                {error, _Reason, _Ctx3} = Error ->
                    Error
            end;
        _ ->
            {not_found, Ctx}
    end.

% https://tools.ietf.org/html/rfc5440#section-7.3
encode_object(Ctx, #pcep_obj_open{} = Obj) ->
    #pcep_obj_open{
        flag_p = P,
        flag_i = I,
        keepalive = Keepalive,
        dead_timer = DeadTimer,
        sid = SID,
        tlvs = TLVs
    } = Obj,
    Body = <<?PCEP_VER:3, 0:5, Keepalive:8, DeadTimer:8, SID:8>>,
    {ok, P, I, Body, 4, TLVs, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.4
% https://tools.ietf.org/html/rfc5541#section-3.3
encode_object(Ctx, #pcep_obj_rp{} = Obj) ->
    #pcep_obj_rp{
        flag_p = P,
        flag_i = I,
        flag_o = O,
        flag_b = B,
        flag_r = R,
        flag_s = S,
        pri = Pri,
        req_id = ReqId,
        tlvs = TLVs
    } = Obj,
    Body = <<0:24, (bool2bit(S)):1, 0:1, (bool2bit(O)):1, (bool2bit(B)):1,
             (bool2bit(R)):1, Pri:3, ReqId:32>>,
    {ok, P, I, Body, 8, TLVs, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.5
encode_object(Ctx, #pcep_obj_nopath{} = Obj) ->
    #pcep_obj_nopath{
        flag_p = P,
        flag_i = I,
        flag_c = C,
        ni = Nature,
        tlvs = TLVs
    } = Obj,
    NI = pcep_codec_iana:pcep_nopath_nature(Nature),
    Body = <<NI:8, (bool2bit(C)):1, 0:15, 0:8>>,
    {ok, P, I, Body, 4, TLVs, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.6
encode_object(Ctx, #pcep_obj_endpoint_ipv4_addr{} = Obj) ->
    #pcep_obj_endpoint_ipv4_addr{
        flag_p = P,
        flag_i = I,
        source = Src,
        destination = Dst
    } = Obj,
    Body = [addr4(Src), addr4(Dst)],
    {ok, P, I, Body, 8, [], Ctx};
encode_object(Ctx, #pcep_obj_endpoint_ipv6_addr{} = Obj) ->
    #pcep_obj_endpoint_ipv6_addr{
        flag_p = P,
        flag_i = I,
        source = Src,
        destination = Dst
    } = Obj,
    Body = [addr6(Src), addr6(Dst)],
    {ok, P, I, Body, 32, [], Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.7
encode_object(Ctx, #pcep_obj_bandwidth_req{} = Obj) ->
    #pcep_obj_bandwidth_req{flag_p = P, flag_i = I, bandwidth = B} = Obj,
    {ok, P, I, <<B:32/float>>, 4, [], Ctx};
encode_object(Ctx, #pcep_obj_bandwidth_telsp{} = Obj) ->
    #pcep_obj_bandwidth_telsp{flag_p = P, flag_i = I, bandwidth = B} = Obj,
    {ok, P, I, <<B:32/float>>, 4, [], Ctx};
encode_object(Ctx, #pcep_obj_bandwidth_cisco{} = Obj) ->
    #pcep_obj_bandwidth_cisco{flag_p = P, flag_i = I, bandwidth = B} = Obj,
    {ok, P, I, <<B:32/float>>, 4, [], Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.8
encode_object(Ctx, #pcep_obj_metric{} = Obj) ->
    #pcep_obj_metric{
        flag_p = P,
        flag_i = I,
        flag_c = C,
        flag_b = B,
        type = Type,
        value = V
    } = Obj,
    T = pcep_codec_iana:pcep_metric_type(Type),
    Body = <<0:16, 0:6, (bool2bit(C)):1, (bool2bit(B)):1, T:8, V:32/float>>,
    {ok, P, I, Body, 8, [], Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.9
% https://tools.ietf.org/html/rfc3209#section-4.3.2
encode_object(Ctx, #pcep_obj_ero{} = Obj) ->
    #pcep_obj_ero{flag_p = P, flag_i = I, path = SubObjs} = Obj,
    encode_ro(Ctx, P, I, SubObjs);
% https://tools.ietf.org/html/rfc5440#section-7.10
% https://tools.ietf.org/html/rfc3209#section-4.3.2
encode_object(Ctx, #pcep_obj_rro{} = Obj) ->
    #pcep_obj_rro{flag_p = P, flag_i = I, path = SubObjs} = Obj,
    encode_ro(Ctx, P, I, SubObjs);
% https://tools.ietf.org/html/rfc5440#section-7.12
% https://tools.ietf.org/html/rfc3209#section-4.3.2
encode_object(Ctx, #pcep_obj_iro{} = Obj) ->
    #pcep_obj_iro{flag_p = P, flag_i = I, path = SubObjs} = Obj,
    encode_ro(Ctx, P, I, SubObjs);
% https://tools.ietf.org/html/rfc5440#section-7.11
encode_object(Ctx, #pcep_obj_lspa{} = Obj) ->
    #pcep_obj_lspa{
        flag_p = P,
        flag_i = I,
        flag_l = L,
        exclude_any = ExcAny,
        include_any = IncAny,
        include_all = IncAll,
        setup_prio = SetupPrio,
        holding_prio = HoldPrio,
        tlvs = TLVs
    } = Obj,
    Body = <<ExcAny:32, IncAny:32, IncAll:32, SetupPrio:8, HoldPrio:8,
             0:7, (bool2bit(L)):1, 0:8>>,
    {ok, P, I, Body, 16, TLVs, Ctx};
% TODO: SVEC https://tools.ietf.org/html/rfc5440#section-7.13.2
% https://tools.ietf.org/html/rfc5440#section-7.14
encode_object(Ctx, #pcep_obj_notif{} = Obj) ->
    #pcep_obj_notif{
        flag_p = P,
        flag_i = I,
        type = Type,
        value = Value,
        tlvs = TLVs
    } = Obj,
    {T, V} = pcep_codec_iana:pcep_notif(Type, Value),
    Body = <<0:16, T:8, V:8>>,
    {ok, P, I, Body, 4, TLVs, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.15
encode_object(Ctx, #pcep_obj_error{} = Obj) ->
    #pcep_obj_error{
        flag_p = P,
        flag_i = I,
        type = Type,
        value = Value,
        tlvs = TLVs
    } = Obj,
    {T, V} = pcep_codec_iana:pcep_error(Type, Value),
    Body = <<0:16, T:8, V:8>>,
    {ok, P, I, Body, 4, TLVs, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.16
encode_object(Ctx, #pcep_obj_load_balancing{} = Obj) ->
    #pcep_obj_load_balancing{
        flag_p = P,
        flag_i = I,
        max_lsp = M,
        min_bandwidth = B
    } = Obj,
    Body = <<0:16, 0:8, M:8, B:32/float>>,
    {ok, P, I, Body, 8, [], Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.17
encode_object(Ctx, #pcep_obj_close{} = Obj) ->
    #pcep_obj_close{
        flag_p = P,
        flag_i = I,
        reason = Reason,
        tlvs = TLVs
    } = Obj,
    R = pcep_codec_iana:pcep_close_reason(Reason),
    Body = <<0:16, 0:8, R:8>>,
    {ok, P, I, Body, 4, TLVs, Ctx};
% https://tools.ietf.org/html/rfc8231#section-7.2
encode_object(Ctx, #pcep_obj_srp{} = Obj) ->
    #pcep_obj_srp{
        flag_p = P,
        flag_i = I,
        srp_id = ID,
        tlvs = TLVs
    } = Obj,
    Body = <<0:32, ID:32>>,
    {ok, P, I, Body, 8, TLVs, Ctx};
% https://tools.ietf.org/html/rfc8231#section-7.3
% https://tools.ietf.org/html/rfc8281#section-8.2
encode_object(Ctx, #pcep_obj_lsp{} = Obj) ->
    #pcep_obj_lsp{
        flag_p = P,
        flag_i = I,
        flag_d = D,
        flag_s = S,
        flag_r = R,
        flag_a = A,
        flag_c = C,
        plsp_id = PLSPID,
        status = OpStatus,
        tlvs = TLVs
    } = Obj,
    O = pcep_codec_iana:pcep_opstatus(OpStatus),
    Body = <<PLSPID:20, 0:4, (bool2bit(C)):1, O:3, (bool2bit(A)):1,
             (bool2bit(R)):1, (bool2bit(S)):1, (bool2bit(D)):1>>,
    {ok, P, I, Body, 4, TLVs, Ctx};
% Unknown object record
encode_object(Ctx, _Obj) ->
    {not_found, Ctx}.

encode_ro(Ctx, P, I, Objs) ->
    case encode_ro_subobjs(Ctx, 0, [], Objs) of
        {ok, Data, Size, Ctx2} ->
            {ok, P, I, Data, Size, [], Ctx2};
        {error, _Reason, _Ctx2} = Error ->
            Error
    end.

encode_ro_subobjs(Ctx, Size, Acc, []) ->
    {ok, lists:reverse(Acc), Size, Ctx};
encode_ro_subobjs(Ctx, Size, Acc, [Obj | Rest]) ->
    case encode_ro_subobj(Ctx, Obj) of
        {error, _Reason, _Ctx2} = Error -> Error;
        {ok, SubData, SubSize, Ctx2} ->
            encode_ro_subobjs(Ctx2, Size + SubSize, [SubData | Acc], Rest)
    end.

% https://tools.ietf.org/html/rfc3209#section-4.3.2
pack_ro_subobj(Ctx, Type, L, Size, Data) ->
    T = pcep_codec_iana:pcep_ro_subobj_type(Type),
    {ok, [<<(bool2bit(L)):1, T:7, (Size + 2):8>>, Data], Size + 2, Ctx}.

% https://tools.ietf.org/html/rfc3209#section-4.3.3.2
encode_ro_subobj(Ctx, #pcep_ro_subobj_ipv4{} = Obj) ->
    Ctx2 = pcep_codec_context:begin_subobj(Ctx, ipv4),
    #pcep_ro_subobj_ipv4{
        flag_l = L,
        prefix = {Addr, PS}
    } = Obj,
    case PS =< 32 of
        false ->
            pcep_codec_error:malformed_ro_subobj(Ctx2,
                "invalid IPv4 prefix size ~w", [PS]);
        true ->
            Body = [addr4(Addr), <<PS:8, 0:8>>],
            pack_ro_subobj(Ctx2, ipv4, L, 6, Body)
    end;
% https://tools.ietf.org/html/rfc3209#section-4.3.3.3
encode_ro_subobj(Ctx, #pcep_ro_subobj_ipv6{} = Obj) ->
    Ctx2 = pcep_codec_context:begin_subobj(Ctx, ipv6),
    #pcep_ro_subobj_ipv6{
        flag_l = L,
        prefix = {Addr, PS}
    } = Obj,
    case PS =< 128 of
        false ->
            pcep_codec_error:malformed_ro_subobj(Ctx2,
                "invalid IPv6 prefix size ~w", [PS]);
        true ->
            Body = [addr6(Addr), <<PS:8, 0:8>>],
            pack_ro_subobj(Ctx2, ipv6, L, 18, Body)
    end;
% https://tools.ietf.org/html/rfc3477#section-4
encode_ro_subobj(Ctx, #pcep_ro_subobj_unum{} = Obj) ->
    Ctx2 = pcep_codec_context:begin_subobj(Ctx, unum),
    #pcep_ro_subobj_unum{
        flag_l = L,
        router_id = RouterId,
        interface_id = I
    } = Obj,
    Body = [<<0:16>>, addr4(RouterId), <<I:32>>],
    pack_ro_subobj(Ctx2, unum, L, 10, Body);
% https://tools.ietf.org/html/rfc3209#section-4.3.3.4
encode_ro_subobj(Ctx, #pcep_ro_subobj_asn{} = Obj) ->
    Ctx2 = pcep_codec_context:begin_subobj(Ctx, asn),
    #pcep_ro_subobj_asn{flag_l = L, asn = ASN} = Obj,
    pack_ro_subobj(Ctx2, asn, L, 2, <<ASN:16>>);
% https://tools.ietf.org/html/rfc8664#section-4.3.1
encode_ro_subobj(Ctx, #pcep_ro_subobj_sr{} = Obj) ->
    Ctx2 = pcep_codec_context:begin_subobj(Ctx, sr),
    #pcep_ro_subobj_sr{
        flag_l = L,
        has_sid = HasSID,
        has_nai = HasNAI,
        nai_type = NAIType,
        is_mpls = IsMPLS,
        has_mpls_extra = HasExtra
    } = Obj,
    NT = pcep_codec_iana:pcep_nai_type(NAIType),
    F = bool2bit(not HasNAI),
    S = bool2bit(not HasSID),
    M = bool2bit(IsMPLS),
    C = bool2bit(HasExtra),
    case encode_sr_ro_sid(Ctx2, Obj) of
        {error, _Reason, _Ctx3} = Error -> Error;
        {ok, SIDBin, SIDSize, Ctx3} ->
            case encode_sr_ro_nai(Ctx3, Obj) of
                {error, _Reason, _Ctx4} = Error -> Error;
                {ok, NAIBin, NAISize, Ctx4} ->
                    Body = [<<NT:4, 0:8, F:1, S:1, C:1, M:1>>, SIDBin, NAIBin],
                    Size = SIDSize + NAISize + 2,
                    pack_ro_subobj(Ctx4, sr, L, Size, Body)
            end
    end.

encode_sr_ro_sid(Ctx, #pcep_ro_subobj_sr{has_sid = false}) ->
    {ok, <<>>, 0, Ctx};
encode_sr_ro_sid(Ctx, #pcep_ro_subobj_sr{is_mpls = true,
                        sid = #mpls_stack_entry{label = L}}) ->
    {ok, <<L:20, 0:3, 0:1, 0:8>>, 4, Ctx};
encode_sr_ro_sid(Ctx, #pcep_ro_subobj_sr{is_mpls = true,
                        sid = #mpls_stack_entry_ext{} = SID}) ->
    #mpls_stack_entry_ext{label = L, tc = TC, is_last = S, ttl = TTL} = SID,
    {ok, <<L:20, TC:3, (bool2bit(S)):1, TTL:8>>, 4, Ctx};
encode_sr_ro_sid(Ctx, #pcep_ro_subobj_sr{sid = SID})
  when is_integer(SID) ->
    {ok, <<SID:32>>, 4, Ctx};
encode_sr_ro_sid(Ctx, _Obj) ->
    pcep_codec_error:malformed_ro_subobj(Ctx,
        "invalid SR subobject, malformed or missing SID", []).

encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{has_nai = false}) ->
    {ok, <<>>, 0, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = ipv4_node,
                                         nai = #srte_nai_node{} = NAI}) ->
    #srte_nai_node{address = Addr} = NAI,
    {ok, addr4(Addr), 4, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = ipv6_node,
                                         nai = #srte_nai_node{} = NAI}) ->
    #srte_nai_node{address = Addr} = NAI,
    {ok, addr6(Addr), 16, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = ipv4_adjacency,
                                         nai = #srte_nai_adjacency{} = NAI}) ->
    #srte_nai_adjacency{local_address = LAddr, remote_address = RAddr} = NAI,
    {ok, [addr4(LAddr), addr4(RAddr)], 8, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = ipv6_adjacency,
                                         nai = #srte_nai_adjacency{} = NAI}) ->
    #srte_nai_adjacency{local_address = LAddr, remote_address = RAddr} = NAI,
    {ok, [addr6(LAddr), addr6(RAddr)], 32, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = unumbered_ipv4_adjacency,
                        nai = #srte_nai_unumbered_ipv4_adjacency{} = NAI}) ->
    #srte_nai_unumbered_ipv4_adjacency{
        local_node_id = LNId,
        local_interface_id =LIId,
        remote_node_id = RNId,
        remote_interface_id = RIId
    } = NAI,
    {ok, <<LNId:32, LIId:32, RNId: 32, RIId:32>>, 16, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = linklocal_ipv6_adjacency,
                        nai = #srte_nai_linklocal_ipv6_adjacency{} = NAI}) ->
    #srte_nai_linklocal_ipv6_adjacency{
        local_address = LAddr,
        local_interface_id = LIId,
        remote_address = RAddr,
        remote_interface_id = RIId
    } = NAI,
    {ok, [addr6(LAddr), <<LIId:32>>, addr6(RAddr), <<RIId:32>>], 40, Ctx};
encode_sr_ro_nai(Ctx, #pcep_ro_subobj_sr{nai_type = NAIType}) ->
    pcep_codec_error:malformed_ro_subobj(Ctx,
        "invalid SR subobject, malformed ~w NAI", [NAIType]).
