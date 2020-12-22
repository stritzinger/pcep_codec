-module(pcep_codec_error).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec_iana.hrl").
-include("pcep_codec_internal.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([throw_internal_error/5]).
-export([throw_not_implemented/3]).

-export([missing_mandatory_object/2]).

-export([malformed_message/3]).
-export([version_not_supported/2]).
-export([unknown_message/2]).
-export([unknown_object_class/3]).
-export([unknown_object_type/4]).
-export([malformed_object/1]).
-export([malformed_tlv/1]).
-export([malformed_sub_tlv/1]).
-export([malformed_ro_subobj/3]).
-export([capability_not_supported/3]).
-export([unknown_notification_type/3]).
-export([unknown_notification_value/4]).
-export([unknown_error_type/3]).
-export([unknown_error_value/4]).
-export([unknown_close_reason/2]).
-export([unknown_lsp_error/2]).
-export([unknown_pst/2]).
-export([unknown_sr_nai_type/2]).
-export([unknown_lsp_opstatus/2]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

throw_internal_error(Ctx, Fmt, Params, File, Line) ->
    Info = fmt("Unexpected internal error at ~s:~w: " ++ Fmt ,
               [filename:basename(File), Line | Params]),
    Error = #{
        type => internal_error,
        critical => true,
        info => Info,
        file => File,
        line => Line,
        path => pcep_codec_context:get_path(Ctx)
    },
    Ctx2 = set_error(Ctx, Error, undefined),
    throw({pcep_codec_error, internal_error, Ctx2}).

throw_not_implemented(Ctx, File, Line) ->
    Info = fmt("Functionality not implemented yet in ~s:~w",
               [filename:basename(File), Line]),
    Error = #{
        type => not_implemented,
        critical => true,
        info => Info,
        file => File,
        line => Line,
        path => pcep_codec_context:get_path(Ctx)
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    throw({pcep_codec_error, not_implemented, Ctx2}).

missing_mandatory_object(Ctx, Class) ->
    MissingErrValMap = #{
        rp_class => rp_object_missing,
        rro_class => rro_object_missing_for_reop,
        endpoint_class => ep_object_missing,
        lsp_class => lsp_object_missing,
        ero_class => ero_object_missing,
        srp_class => srp_object_missing
    },
    ErrVal = maps:get(Class, MissingErrValMap, undefined),
    Error = #{
        type => missing_object,
        critical => false,
        info => fmt("Missing required PCEP object ~w", [Class]),
        path => pcep_codec_context:get_path(Ctx),
        object_class => Class,
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data)
    },
    set_error(Ctx, Error, ErrVal).

malformed_message(Ctx, Fmt, Args) ->
    MsgType = pcep_codec_context:get(Ctx, message_type),
    Info = fmt("Malformed message ~w: " ++ Fmt, [MsgType | Args]),
    Error = #{
        type => malformed_message,
        critical => true,
        info => Info,
        message_type => MsgType
    },
    {error, malformed_message, set_error(Ctx, Error, undefined)}.

version_not_supported(Ctx, PcepVer) ->
    Error = #{
        type => pcep_version_not_supported,
        critical => true,
        info => fmt("PCEP version ~w not supported", [PcepVer])
    },
    Ctx2 = set_error(Ctx, Error, pcep_version_not_supported),
    {error, pcep_version_not_supported, Ctx2}.

unknown_message(Ctx, PcepMsgType) ->
    Error = #{
        type => unknown_message,
        critical => false,
        info => fmt("Unknown PCEP message type ~w", [PcepMsgType]),
        message_data => pcep_codec_context:get(Ctx, message_data)
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_message, Ctx2}.

unknown_object_class(Ctx, PcepClassId, PcepTypeId) ->
    Info = fmt("Unsupported PCEP object with class ID ~w and type ID ~w",
               [PcepClassId, PcepTypeId]),
    Error = #{
        type => unknown_object_class,
        info => Info,
        critical => false,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_object_class => PcepClassId,
        pcep_object_type => PcepTypeId
    },
    {error, unknown_object_class, set_error(Ctx, Error, unrec_object_class)}.

unknown_object_type(Ctx, Class, PcepClassId, PcepTypeId) ->
    Info = fmt("Unsupported PCEP object type ID ~w for class ~w",
               [PcepTypeId, Class]),
    Error = #{
        type => unknown_object_type,
        info => Info,
        critical => false,
        path => pcep_codec_context:get_path(Ctx),
        object_class => Class,
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_object_class => PcepClassId,
        pcep_object_type => PcepTypeId
    },
    {error, unknown_object_type, set_error(Ctx, Error, unrec_object_type)}.

malformed_object(Ctx) ->
    Error = #{
        type => malformed_object,
        critical => true,
        info => "Malformed PCEP object",
        path => pcep_codec_context:get_path(Ctx),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data)
    },
    {error, malformed_object, set_error(Ctx, Error, malformed_object)}.

malformed_tlv(Ctx) ->
    Info = fmt("Malformed PCEP TLV ~w",
               [pcep_codec_context:get(Ctx, tlv_type)]),
    Error = #{
        type => malformed_tlv,
        critical => true,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        tlv_type => pcep_codec_context:get(Ctx, tlv_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        tlv_data => pcep_codec_context:get(Ctx, tlv_data)
    },
    {error, malformed_tlv, set_error(Ctx, Error, malformed_object)}.

malformed_sub_tlv(Ctx) ->
    Info = fmt("Malformed PCEP Sub-TLV ~w",
               [pcep_codec_context:get(Ctx, sub_tlv_type)]),
    Error = #{
        type => malformed_sub_tlv,
        critical => true,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        tlv_type => pcep_codec_context:get(Ctx, tlv_type),
        sub_tlv_type => pcep_codec_context:get(Ctx, sub_tlv_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        tlv_data => pcep_codec_context:get(Ctx, tlv_data)
    },
    {error, malformed_sub_tlv, set_error(Ctx, Error, malformed_object)}.

malformed_ro_subobj(Ctx, Fmt, Args) ->
    Info = fmt("Malformed RO sub-object ~w: " ++ Fmt,
                [pcep_codec_context:get(Ctx, subobj_type) | Args]),
    Error = #{
        type => malformed_ro_subobj,
        critical => true,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        subobj_type => pcep_codec_context:get(Ctx, subobj_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        subobj_data => pcep_codec_context:get(Ctx, subobj_data)
    },
    {error, malformed_ro_subobj, set_error(Ctx, Error, malformed_object)}.

capability_not_supported(Ctx, Fmt, Args) ->
    Info = fmt("Capability not supported: " ++ Fmt, Args),
    Error = #{
        type => capability_not_supported,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        tlv_data => pcep_codec_context:get(Ctx, tlv_data)
    },
    {error, capability_not_supported,
        set_error(Ctx, Error, capability_not_supported)}.

unknown_notification_type(Ctx, TypeId, ValueId) ->
    Info = fmt("Unsupported PCEP notification with type ID ~w and value ID ~w",
               [TypeId, ValueId]),
    Error = #{
        type => unknown_notif_type,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_notif_type => TypeId,
        pcep_notif_value => ValueId
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_notif_type, Ctx2}.

unknown_notification_value(Ctx, Type, TypeId, ValueId) ->
    Info = fmt("Unsupported PCEP notification with value ID ~w for type ~w",
               [ValueId, Type]),
    Error = #{
        type => unknown_notif_value,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        notif_type => Type,
        pcep_notif_type => TypeId,
        pcep_notif_value => ValueId
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_notif_value, Ctx2}.

unknown_error_type(Ctx, TypeId, ValueId) ->
    Info = fmt("Unsupported PCEP error with type ID ~w and value ID ~w",
               [TypeId, ValueId]),
    Error = #{
        type => unknown_error_type,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_notif_type => TypeId,
        pcep_notif_value => ValueId
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_error_type, Ctx2}.

unknown_error_value(Ctx, Type, TypeId, ValueId) ->
    Info = fmt("Unsupported PCEP error with value ID ~w for type ~w",
               [ValueId, Type]),
    Error = #{
        type => unknown_error_value,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        notif_type => Type,
        pcep_notif_type => TypeId,
        pcep_notif_value => ValueId
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_error_value, Ctx2}.

unknown_close_reason(Ctx, ReasonId) ->
    Info = fmt("Unsupported PCEP close reason with ID ~w", [ReasonId]),
    Error = #{
        type => unknown_close_reason,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_close_reason => ReasonId
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_close_reason, Ctx2}.

unknown_lsp_error(Ctx, Code) ->
    Info = fmt("Unsupported PCEP LSP error code ~w", [Code]),
    Error = #{
        type => unknown_lsp_error,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_lsp_error => Code
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_lsp_error, Ctx2}.

unknown_pst(Ctx, Code) ->
    Info = fmt("Unsupported PST code ~w", [Code]),
    Error = #{
        type => unknown_pst_error,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_pst => Code
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_pst_error, Ctx2}.

unknown_sr_nai_type(Ctx, Code) ->
    Info = fmt("Unsupported SR NAI type code ~w", [Code]),
    Error = #{
        type => unknown_sr_nai_type_error,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        subobj_type => pcep_codec_context:get(Ctx, subobj_type),
        subobj_data => pcep_codec_context:get(Ctx, subobj_data),
        pcep_nai_type => Code
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_sr_nai_type_error, Ctx2}.

unknown_lsp_opstatus(Ctx, Code) ->
    Info = fmt("Unsupported LSP operational status code ~w", [Code]),
    Error = #{
        type => unknown_lsp_opstatus,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_opstatus => Code
    },
    Ctx2 = set_error(Ctx, Error, capability_not_supported),
    {error, unknown_lsp_opstatus, Ctx2}.


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

set_error(Ctx, Error, undefined) ->
    pcep_codec_context:set_error(Ctx, Error);
set_error(Ctx, Error, Value) ->
    {TypeId, ValueId} = pcep_codec_iana:pcep_error(Value),
    Type =  pcep_codec_iana:error_type_from_value(Value),
    Error2 = Error#{
        error_type => Type,
        error_value => Value,
        pcep_error_type => TypeId,
        pcep_error_value => ValueId
    },
    pcep_codec_context:set_error(Ctx, Error2).