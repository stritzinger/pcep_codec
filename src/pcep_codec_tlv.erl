-module(pcep_codec_tlv).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([decode/2]).
-export([encode/2]).


%%% IMPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-import(pcep_codec_utils, [bit2bool/1]).
-import(pcep_codec_utils, [bool2bit/1]).
-import(pcep_codec_utils, [addr4/1, addr6/1]).
-import(pcep_codec_utils, [padding/1]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Ctx, Data) ->
    decode_tlvs(Ctx, [], Data).

encode(Ctx, TLVs) ->
    encode_tlvs(Ctx, TLVs).

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-- Decoding Functions ---------------------------------------------------------

decode_tlvs(Ctx, Acc, <<>>) ->
    {ok, lists:reverse(Acc), Ctx};
decode_tlvs(Ctx, Acc, Data) ->
    case decode_tlv_header(Ctx, Data) of
        {ok, PcepType, Len, Body, Rem, Ctx2} ->
            <<TlvData:(Len+4)/binary, _/binary>> = Data,
            Ctx3 = Ctx2#{tlv_data => TlvData},
            case pcep_codec_iana:int_tlv_type(PcepType) of
                undefined ->
                    Ctx4 = pcep_codec_context:ignore_tlv(Ctx3),
                    Ctx5 = pcep_codec_warning:unknown_tlv_type(Ctx4, PcepType),
                    Tlv = #pcep_tlv_unknown{type = PcepType, data = Body},
                    decode_tlvs(Ctx5, [Tlv | Acc], Rem);
                Type ->
                    Ctx4 = pcep_codec_context:begin_tlv(Ctx3, Type),
                    case decode_tlv(Ctx4, Type, Body) of
                        {ok, Tlv, Ctx5} ->
                            decode_tlvs(Ctx5, [Tlv | Acc], Rem);
                        {error, _Reason, _Ctx5} = Result ->
                            Result
                    end
            end;
        {error, _Reason, _Ctx2} = Result ->
            Result
    end.

decode_tlv_header(Ctx, <<Type:16, 0:16, Rem/binary>>) ->
    {ok, Type, 0, <<>>, Rem, Ctx};
decode_tlv_header(Ctx, <<Type:16, Len:16, Body:Len/binary,
                         _Padding:(3 - ((Len - 1) rem 4))/binary,
                         Rem/binary>>) ->
    {ok, Type, Len, Body, Rem, Ctx};
decode_tlv_header(Ctx, _Data) ->
    pcep_codec_error:malformed_tlv(Ctx).

% https://tools.ietf.org/html/rfc5440#section-7.5
decode_tlv(Ctx, nopath_vector, <<_:29, US:1, UD:1, NA:1>>) ->
    {ok, #pcep_tlv_nopath_vector{
        pce_not_available = bit2bool(NA),
        unknown_destination = bit2bool(UD),
        unknown_source = bit2bool(US)
    }, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.14
decode_tlv(Ctx, overload_duration, <<E:32>>) ->
    {ok, #pcep_tlv_overload_duration{estimated = E}, Ctx};
% https://tools.ietf.org/html/rfc5440#section-7.15
decode_tlv(Ctx, req_missing, <<I:32>>) ->
    {ok, #pcep_tlv_req_missing{req_id = I}, Ctx};
%TODO: Implemente TLV objfun_list
decode_tlv(Ctx, objfun_list, _) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV vendor_info
decode_tlv(Ctx, vendor_info, _) ->
    ?NOT_IMPLEMENTED(Ctx);
% https://tools.ietf.org/html/rfc8231#section-7.1.1
% https://tools.ietf.org/html/rfc8281#section-4.1
decode_tlv(Ctx, stateful_pce_cap, <<_:29, I:1, _:1, U:1>>) ->
    {ok, #pcep_tlv_stateful_pce_cap{
        flag_u = bit2bool(U),
        flag_i = bit2bool(I)
    }, Ctx};
% https://tools.ietf.org/html/rfc8231#section-7.3.2
decode_tlv(Ctx, symbolic_path_name, Name) ->
    {ok, #pcep_tlv_symbolic_path_name{name = Name}, Ctx};
% https://tools.ietf.org/html/rfc8231#section-7.3.1
decode_tlv(Ctx, ipv4_lsp_id, <<S1:8, S2:8, S3:8, S4:8, L:16, T:16, X:4/binary,
                               E1:8, E2:8, E3:8, E4:8>>) ->
    {ok, #pcep_tlv_ipv4_lsp_id{
        source = {S1, S2, S3, S4},
        lsp_id = L,
        tunnel_id = T,
        extended_id = X,
        endpoint = {E1, E2, E3, E4}
    }, Ctx};
% https://tools.ietf.org/html/rfc8231#section-7.3.1
decode_tlv(Ctx, ipv6_lsp_id,
           <<S1:16,S2:16,S3:16,S4:16,S5:16,S6:16,S7:16,S8:16,
             L:16, T:16, X:16/binary,
             E1:16,E2:16,E3:16,E4:16,E5:16,E6:16,E7:16,E8:16>>) ->
    {ok, #pcep_tlv_ipv6_lsp_id{
        source = {S1, S2, S3, S4, S5, S6, S7, S8},
        lsp_id = L,
        tunnel_id = T,
        extended_id = X,
        endpoint = {E1, E2, E3, E4, E5, E6, E7, E8}
    }, Ctx};
% https://tools.ietf.org/html/rfc8231#section-7.3.3
decode_tlv(Ctx, lsp_error_code, <<C:32>>) ->
    case pcep_codec_iana:int_lsp_error(C) of
        undefined ->
            pcep_codec_error:unknown_lsp_error(Ctx, C);
        Error ->
            {ok, #pcep_tlv_lsp_error_code{error = Error}, Ctx}
    end;
% https://tools.ietf.org/html/rfc8231#section-7.3.4
%TODO: Implemente TLV rsvp_error_spec
decode_tlv(Ctx, rsvp_error_spec, _) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV lsp_db_version
decode_tlv(Ctx, lsp_db_version, _) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV speaker_entity_id
decode_tlv(Ctx, speaker_entity_id, _) ->
    ?NOT_IMPLEMENTED(Ctx);
% https://tools.ietf.org/html/draft-ietf-pce-segment-routing-07#section-5.1.1
decode_tlv(Ctx, sr_pce_cap, <<_:16, _:8, MSD:8>>) ->
    {ok, #pcep_tlv_sr_pce_cap{msd = MSD}, Ctx};
% https://tools.ietf.org/html/rfc8408#section-4
decode_tlv(Ctx, path_setup_type, <<_:24, PSTCode:8>>) ->
    case decode_pst(Ctx, PSTCode) of
        {ok, PST, Ctx2} ->
            {ok, #pcep_tlv_path_setup_type{pst = PST}, Ctx2};
        {error, _REason, _Ctx2} = Result ->
            Result
    end;
%TODO: Implemente TLV assoc_range
decode_tlv(Ctx, assoc_range, _) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV global_assoc_source
decode_tlv(Ctx, global_assoc_source, _) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV ext_assoc_id
decode_tlv(Ctx, ext_assoc_id, _) ->
    ?NOT_IMPLEMENTED(Ctx);
% https://tools.ietf.org/html/rfc8408#section-3
decode_tlv(Ctx, path_setup_type_cap,
           <<_:24, N:8, PSTsBin:N/binary,
             _Padding:(3 - ((N - 1) rem 4))/binary,
             SubTLVsBin/binary>>) ->
    case decode_psts(Ctx, PSTsBin) of
        {ok, PSTs, Ctx2} ->
            case decode_sub_tlvs(Ctx2, path_setup_type_cap,
                                 fun decode_sub_tlv_path_setup_type/3,
                                 SubTLVsBin) of
                {ok, SubTLVs, Ctx3} ->
                    %TODO: Validate the subtlvs as defined in https://tools.ietf.org/html/rfc8408#section-3
                    TLV = #pcep_tlv_path_setup_type_cap{psts = PSTs,
                                                        subtlvs = SubTLVs},
                    {ok, TLV, Ctx3};
                {error, _Reason, _Ctx3} = Result ->
                    Result
            end;
        {error, _Reason, _Ctx2} = Result ->
            Result
    end;
%TODO: Implemente TLV assoc_type_list
decode_tlv(Ctx, assoc_type_list, _) ->
    ?NOT_IMPLEMENTED(Ctx);
% Custom CISCO TLV for binding label
decode_tlv(Ctx, cisco_binding_label, <<_:16, L:32>>) ->
    {ok, #pcep_tlv_cisco_binding_label{label = L bsr 12}, Ctx};
% Arbitrary TLV
decode_tlv(Ctx, arbitrary, Data) ->
    %TODO: check maximum data size ?
    {ok, #pcep_tlv_arbitrary{data = Data}, Ctx};
decode_tlv(Ctx, _Type, _Data) ->
    pcep_codec_error:malformed_tlv(Ctx).

decode_sub_tlvs(Ctx, TypeDecodingFun, BodyDecodingFun, Data) ->
    decode_sub_tlvs(Ctx, TypeDecodingFun, BodyDecodingFun, [], Data).

decode_sub_tlvs(Ctx, _, _, Acc, <<>>) ->
    {ok, lists:reverse(Acc), Ctx};
decode_sub_tlvs(Ctx, ParentType, BodyDecodingFun, Acc, Data) ->
    case decode_tlv_header(Ctx, Data) of
        {ok, PcepType, Len, Body, Rem, Ctx2} ->
            <<SubTlvData:(Len+4)/binary, _/binary>> = Data,
            Ctx3 = Ctx2#{sub_tlv_data => SubTlvData},
            case pcep_codec_iana:int_sub_tlv_type(ParentType, PcepType) of
                undefined ->
                    Ctx4 = pcep_codec_context:ignore_sub_tlv(Ctx3),
                    Ctx5 = pcep_codec_warning:unknown_sub_tlv_type(Ctx4, PcepType),
                    SubTlv = #pcep_subtlv_unknown{type = PcepType, data = Body},
                    decode_sub_tlvs(Ctx5, ParentType, BodyDecodingFun,
                                    [SubTlv | Acc], Rem);
                Type ->
                    Ctx4 = pcep_codec_context:begin_sub_tlv(Ctx3, Type),
                    case BodyDecodingFun(Ctx4, Type, Body) of
                        {ok, SubTlv, Ctx5} ->
                            decode_sub_tlvs(Ctx5, ParentType, BodyDecodingFun,
                                            [SubTlv | Acc], Rem);
                        {error, _Reason, _Ctx5} = Result ->
                            Result
                    end
            end;
        {error, _Reason, _Ctx2} = Result ->
            Result
    end.

decode_psts(Ctx, Data) ->
    decode_psts(Ctx, [], Data).

decode_psts(Ctx, Acc, <<>>) ->
    {ok, lists:reverse(Acc), Ctx};
decode_psts(Ctx, Acc, <<PSTCode:8, Rest/binary>>) ->
    case decode_pst(Ctx, PSTCode) of
        {ok, PST, Ctx2} ->
            case lists:member(PST, Acc) of
                true -> decode_psts(Ctx2, Acc, Rest);
                false -> decode_psts(Ctx2, [PST | Acc], Rest)
            end;
        {error, _Reason, _Ctx2} = Result ->
            Result
    end.

decode_pst(Ctx, Code) ->
    case pcep_codec_iana:int_pst(Code) of
        undefined -> pcep_codec_error:unknown_pst(Ctx, Code);
        Name -> {ok, Name, Ctx}
    end.

decode_sub_tlv_path_setup_type(Ctx, path_setup_type_cap_sr,
                               <<_:16, _:6, N:1, X:1, MSD:8>>) ->
    {ok, #pcep_subtlv_path_setup_type_cap_sr{
        flag_n = bit2bool(N),
        flag_x = bit2bool(X),
        msd = MSD
    }, Ctx};
decode_sub_tlv_path_setup_type(Ctx, _Type, _Data) ->
    pcep_codec_error:malformed_sub_tlv(Ctx).


%-- Encoding Functions ---------------------------------------------------------

encode_tlvs(Ctx, TLVs) ->
    encode_tlvs(Ctx, fun pcep_codec_iana:pcep_tlv_type/1,
                fun encode_tlv/2, 0, [], TLVs).

encode_tlvs(Ctx, _IdFun, _EncFun, Size, Acc, []) ->
    {ok, lists:reverse(Acc), Size, Ctx};
encode_tlvs(Ctx, IdFun, EncFun, Size, Acc, [TLV | Rem]) ->
    case EncFun(Ctx, TLV) of
        {ok, Type, Bin, TlvSize, Ctx2} ->
            TypeId = tlv_type_id(Type, IdFun),
            Pad = padding(TlvSize),
            PadSize = byte_size(Pad),
            Header = <<TypeId:16, TlvSize:16>>,
            encode_tlvs(Ctx2, IdFun, EncFun, Size + 4 + TlvSize + PadSize,
                        [Pad, Bin, Header | Acc], Rem);
        {error, _Reason, _Ctx2} = Error ->
            Error
    end.

tlv_type_id(Type, IdFun) when is_atom(Type) -> IdFun(Type);
tlv_type_id(Type, _IdFun) when is_integer(Type) -> Type.

% https://tools.ietf.org/html/rfc5440#section-7.5
encode_tlv(Ctx, #pcep_tlv_nopath_vector{} = TLV) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, nopath_vector),
    #pcep_tlv_nopath_vector{
        pce_not_available = NA,
        unknown_destination = UD,
        unknown_source = US
    } = TLV,
    Bin = <<0:29, (bool2bit(US)):1, (bool2bit(UD)):1, (bool2bit(NA)):1>>,
    {ok, nopath_vector, Bin, 4, Ctx2};
% https://tools.ietf.org/html/rfc5440#section-7.14
encode_tlv(Ctx, #pcep_tlv_overload_duration{estimated = E}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, overload_duration),
    {ok, overload_duration, <<E:32>>, 4, Ctx2};
% https://tools.ietf.org/html/rfc5440#section-7.15
encode_tlv(Ctx, #pcep_tlv_req_missing{req_id = I}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, req_missing),
    {ok, req_missing, <<I:32>>, 4, Ctx2};
%TODO: Implemente TLV objfun_list
encode_tlv(Ctx, #pcep_tlv_objfun_list{}) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV vendor_info
encode_tlv(Ctx, #pcep_tlv_vendor_info{}) ->
    ?NOT_IMPLEMENTED(Ctx);
% https://tools.ietf.org/html/rfc8231#section-7.1.1
encode_tlv(Ctx, #pcep_tlv_stateful_pce_cap{flag_u = U, flag_i = I}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, stateful_pce_cap),
    Bin = <<0:29, (bool2bit(I)):1, 0:1, (bool2bit(U)):1>>,
    {ok, stateful_pce_cap, Bin, 4, Ctx2};
% https://tools.ietf.org/html/rfc8231#section-7.3.2
encode_tlv(Ctx, #pcep_tlv_symbolic_path_name{name = Name}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, symbolic_path_name),
    {ok, symbolic_path_name, Name, byte_size(Name), Ctx2};
% https://tools.ietf.org/html/rfc8231#section-7.3.1
encode_tlv(Ctx, #pcep_tlv_ipv4_lsp_id{} = TLV) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, ipv4_lsp_id),
    #pcep_tlv_ipv4_lsp_id{
        source = S,
        lsp_id = L,
        tunnel_id = T,
        extended_id = X,
        endpoint = E
    } = TLV,
    {ok, ipv4_lsp_id,
     [addr4(S), <<L:16, T:16, X:4/binary>>, addr4(E)], 16, Ctx2};
% https://tools.ietf.org/html/rfc8231#section-7.3.1
encode_tlv(Ctx, #pcep_tlv_ipv6_lsp_id{} = TLV) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, ipv6_lsp_id),
    #pcep_tlv_ipv6_lsp_id{
        source = S,
        lsp_id = L,
        tunnel_id = T,
        extended_id = X,
        endpoint = E
    } = TLV,
    {ok, ipv6_lsp_id,
     [addr6(S), <<L:16, T:16, X:16/binary>>, addr6(E)], 52, Ctx2};
% https://tools.ietf.org/html/rfc8231#section-7.3.3
encode_tlv(Ctx, #pcep_tlv_lsp_error_code{error = Error}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, lsp_error_code),
    {ok, lsp_error_code,
     <<(pcep_codec_iana:pcep_lsp_error(Error)):32>>, 4, Ctx2};
% https://tools.ietf.org/html/rfc8231#section-7.3.4
%TODO: Implemente TLV rsvp_error_spec
encode_tlv(Ctx, #pcep_tlv_rsvp_error_spec{}) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV lsp_db_version
encode_tlv(Ctx, #pcep_tlv_lsp_db_version{}) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV speaker_entity_id
encode_tlv(Ctx, #pcep_tlv_speaker_entity_id{}) ->
    ?NOT_IMPLEMENTED(Ctx);
% https://tools.ietf.org/html/draft-ietf-pce-segment-routing-07#section-5.1.1
encode_tlv(Ctx, #pcep_tlv_sr_pce_cap{msd = MSD}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, sr_pce_cap),
    {ok, sr_pce_cap, <<0:16, 0:8, MSD:8>>, 4, Ctx2};
% https://tools.ietf.org/html/rfc8408#section-4
encode_tlv(Ctx, #pcep_tlv_path_setup_type{pst = PST}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, path_setup_type),
    {ok, path_setup_type, <<0:24, (pcep_codec_iana:pcep_pst(PST)):8>>, 4, Ctx2};
%TODO: Implemente TLV assoc_range
encode_tlv(Ctx, #pcep_tlv_assoc_range{}) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV global_assoc_source
encode_tlv(Ctx, #pcep_tlv_global_assoc_source{}) ->
    ?NOT_IMPLEMENTED(Ctx);
%TODO: Implemente TLV ext_assoc_id
encode_tlv(Ctx, #pcep_tlv_ext_assoc_id{}) ->
    ?NOT_IMPLEMENTED(Ctx);
% https://tools.ietf.org/html/rfc8408#section-3
encode_tlv(Ctx, #pcep_tlv_path_setup_type_cap{psts = PSTs, subtlvs = Sub}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, path_setup_type_cap),
    N = length(PSTs),
    Bin1 = <<0:24, N>>,
    Bin2 = << <<(pcep_codec_iana:pcep_pst(V)):8>> || V <- PSTs >>,
    Pad = padding(N),
    Size = 4 + byte_size(Bin2) + byte_size(Pad),
    encode_sub_tlvs(Ctx2, path_setup_type_cap, Size, [Bin1, Bin2, Pad], Sub);
%TODO: Implemente TLV assoc_type_list
encode_tlv(Ctx, #pcep_tlv_assoc_type_list{}) ->
    ?NOT_IMPLEMENTED(Ctx);
% Generic unknown TLV
encode_tlv(Ctx, #pcep_tlv_unknown{type = T, data = D}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, unknown_tlv),
    {ok, T, D, byte_size(D), Ctx2};
% Custom CISCO TLV for binding label
encode_tlv(Ctx, #pcep_tlv_cisco_binding_label{label = L}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, cisco_binding_label),
    {ok, cisco_binding_label, <<0:16, (L bsl 12):32>>, 6, Ctx2};
% Arbitrary data TLV
encode_tlv(Ctx, #pcep_tlv_arbitrary{data = Data}) ->
    Ctx2 = pcep_codec_context:begin_tlv(Ctx, arbitrary),
    {ok, arbitrary, Data, byte_size(Data), Ctx2};
% Unsupported TLV record
encode_tlv(Ctx, Other) ->
    {error, {unsupported_tlv, Other}, Ctx}.

encode_sub_tlvs(Ctx, Type, Size, Data, TLVs) ->
    case encode_tlvs(Ctx, fun pcep_codec_iana:pcep_sub_tlv_type/1,
                     fun encode_sub_tlv/2, 0, [], TLVs) of
        {ok, SubData, SubSize, Ctx2} ->
            {ok, Type, [Data, SubData], Size + SubSize, Ctx2};
        {error, _Reason, _Ctx2} = Error ->
            Error
    end.

% https://tools.ietf.org/html/rfc8664#section-4.1.2
encode_sub_tlv(Ctx, #pcep_subtlv_path_setup_type_cap_sr{} = TLV) ->
    Ctx2 = pcep_codec_context:begin_sub_tlv(Ctx, path_setup_type_cap_sr),
    #pcep_subtlv_path_setup_type_cap_sr{
        flag_n = N,
        flag_x = X,
        msd = MSD
    } = TLV,
    {ok, path_setup_type_cap_sr,
     <<0:16, 0:6, (bool2bit(N)):1, (bool2bit(X)):1, MSD:8>>, 4, Ctx2};
% Generic unknown Sub-TLV
encode_sub_tlv(Ctx, #pcep_subtlv_unknown{type = T, data = D}) ->
    Ctx2 = pcep_codec_context:begin_sub_tlv(Ctx, unknown_subtlv),
    {ok, T, D, byte_size(D), Ctx2}.
