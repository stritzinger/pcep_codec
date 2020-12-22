-module(pcep_codec_warning).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec_iana.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([unknown_object_class/3]).
-export([unknown_object_type/4]).
-export([unknown_tlv_type/2]).
-export([unknown_sub_tlv_type/2]).
-export([capability_not_supported/3]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unknown_object_class(Ctx, PcepClassId, PcepTypeId) ->
    Info = fmt("Unsupported PCEP object with class ID ~w and type ID ~w",
               [PcepClassId, PcepTypeId]),
    Warn = #{
        type => unknown_object_class,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_object_class => PcepClassId,
        pcep_object_type => PcepTypeId,
        pcep_error_type => ?PCEP_ERRT_UNKNOW_OBJECT,
        pcep_error_value => ?PCEP_ERRV_UNREC_OBJECT_CLASS
    },
    pcep_codec_context:add_warning(Ctx, Warn).

unknown_object_type(Ctx, Class, PcepClassId, PcepTypeId) ->
    Info = fmt("Unsupported PCEP object type ID ~w for class ~w",
               [PcepTypeId, Class]),
    Warn = #{
        type => unknown_object_type,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        object_class => Class,
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        pcep_object_class => PcepClassId,
        pcep_object_type => PcepTypeId,
        pcep_error_type => ?PCEP_ERRT_UNKNOW_OBJECT,
        pcep_error_value => ?PCEP_ERRV_UNREC_OBJECT_TYPE
    },
    pcep_codec_context:add_warning(Ctx, Warn).

unknown_tlv_type(Ctx, PcepTypeId) ->
    Info = fmt("Unsupported PCEP TLV type ID ~w", [PcepTypeId]),
    Warn = #{
        type => unknown_tlv_type,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        tlv_data => pcep_codec_context:get(Ctx, tlv_data),
        pcep_tlv_type => PcepTypeId
    },
    pcep_codec_context:add_warning(Ctx, Warn).

unknown_sub_tlv_type(Ctx, PcepTypeId) ->
    Info = fmt("Unsupported PCEP Sub-TLV type ID ~w", [PcepTypeId]),
    Warn = #{
        type => unknown_sub_tlv_type,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_type => pcep_codec_context:get(Ctx, message_type),
        object_class => pcep_codec_context:get(Ctx, object_class),
        object_type => pcep_codec_context:get(Ctx, object_type),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        tlv_data => pcep_codec_context:get(Ctx, tlv_data),
        tlv_type => pcep_codec_context:get(Ctx, tlv_type),
        sub_tlv_data => pcep_codec_context:get(Ctx, sub_tlv_data),
        pcep_sub_tlv_type => PcepTypeId
    },
    pcep_codec_context:add_warning(Ctx, Warn).

capability_not_supported(Ctx, Fmt, Args) ->
    Info = fmt("Capability not supported: " ++ Fmt, Args),
    Warn = #{
        type => capability_not_supported,
        critical => false,
        info => Info,
        path => pcep_codec_context:get_path(Ctx),
        message_data => pcep_codec_context:get(Ctx, message_data),
        object_data => pcep_codec_context:get(Ctx, object_data),
        tlv_data => pcep_codec_context:get(Ctx, tlv_data),
        pcep_error_type => ?PCEP_ERRT_CAPABILITY_NOT_SUPPORTED,
        pcep_error_value => ?PCEP_ERRV_UNASSIGNED
    },
    pcep_codec_context:add_warning(Ctx, Warn).


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).
