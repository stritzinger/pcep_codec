-module(pcep_codec_message).


%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").
-include("pcep_codec_iana.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([decode/2]).
-export([encode/2]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Ctx, <<?PCEP_VER:3, _:5, T:8, L:16, B:(L-4)/binary, R/binary>> = Data) ->
    <<MsgData:L/binary, _/binary>> = Data,
    Ctx2 = Ctx#{message_data => MsgData},
    case pcep_codec_iana:int_message_type(T) of
        undefined ->
            pack_remdata(R, pcep_codec_error:unknown_message(Ctx2, T));
        Type ->
            Ctx3 = pcep_codec_context:begin_message(Ctx2, Type),
            case decode_message(Ctx3, Type, B) of
                {ok, Msg, Ctx4} ->
                    {ok, Msg, R, Ctx4};
                {error, Reason, Ctx4} ->
                    {error, Reason, R, Ctx4}
            end
    end;
decode(Ctx, <<?PCEP_VER:3, _:5, _:8, L:16, _/binary>>) ->
    {more, L, Ctx};
decode(Ctx, <<?PCEP_VER:3, _:5, _/binary>>) ->
    {more, undefined, Ctx};
decode(Ctx, <<Ver:3, _:5, _/binary>> = Data) ->
    pack_remdata(Data, pcep_codec_error:version_not_supported(Ctx, Ver));
decode(Ctx, _Data) ->
    {more, undefined, Ctx}.

encode(Ctx, #pcep_msg_keepalive{}) ->
    pack_message(Ctx, keepalive, 0, <<>>);
encode(Ctx, Msg) ->
    MsgType = message_type(Msg),
    Spec = pcep_codec_specs:get(MsgType),
    case pcep_codec_object:encode_spec(Ctx, Spec, Msg) of
        {error, _Reason, _Ctx2} = Error ->
            Error;
        {not_found, Ctx2} ->
            % This should never happen if the specs are done correctly
            ?INTERNAL_ERROR(Ctx2, "Invalid message, encoding failed", []);
        {ok, Data, Size, Ctx2} ->
            pack_message(Ctx2, MsgType, Size, Data)
    end.


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack_remdata(RemData, {error, Reason, Ctx}) ->
    {error, Reason, RemData, Ctx}.


%-- Decoding Functions ---------------------------------------------------------

decode_message(Ctx, keepalive, _Data) ->
    {ok, #pcep_msg_keepalive{}, Ctx};
decode_message(Ctx, MsgType, Data) ->
    Spec = pcep_codec_specs:get(MsgType),
    Ctx2 = pcep_codec_context:begin_message(Ctx, MsgType),
    case pcep_codec_object:decode_spec(Ctx2, Spec, Data) of
        {ok, Result, RemBin, Ctx3} ->
                case pcep_codec_object:decode(Ctx3, RemBin) of
                    {ok, _, <<>>, Ctx4} ->
                        {ok, Result, Ctx4};
                    {ok, _, _, Ctx4} ->
                        pcep_codec_error:malformed_message(Ctx4,
                            "Unexpected extra data in message", []);
                    {error, _Reason, _Ctx4} = Error ->
                        Error
                end;
        {not_found, Ctx3} ->
            % This should never happen if the specs are done correctly
            ?INTERNAL_ERROR(Ctx3, "Message body not found", []);
        {error, _Reason, _Ctx3} = Result ->
            Result
    end.


%-- Encoding Functions ---------------------------------------------------------

message_type(#pcep_msg_open{}) -> open;
message_type(#pcep_msg_compreq{}) -> compreq;
message_type(#pcep_msg_comprep{}) -> comprep;
message_type(#pcep_msg_notif{}) -> notif;
message_type(#pcep_msg_error{}) -> error;
message_type(#pcep_msg_close{}) -> close;
message_type(#pcep_msg_report{}) -> report;
message_type(#pcep_msg_update{}) -> update;
message_type(#pcep_msg_initiate{}) -> initiate.

pack_message(Ctx, MsgType, Size, Data) ->
    T = pcep_codec_iana:pcep_message_type(MsgType),
    {ok, [<<?PCEP_VER:3, 0:5, T:8, (Size + 4):16>>, Data], Ctx}.
