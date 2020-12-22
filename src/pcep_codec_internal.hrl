

%%% PCEP CONSTANTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PCEP_VER, 1).


%%% ERROR UTILITY MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NOT_IMPLEMENTED(CTX),
    pcep_codec_error:throw_not_implemented(CTX, ?FILE, ?LINE)).
-define(INTERNAL_ERROR(CTX, FMT, PARAMS),
    pcep_codec_error:throw_internal_error(CTX, FMT, PARAMS, ?FILE, ?LINE)).


%%% DEBUG MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -define(ENABLE_DEBUG, true).
-define(DEBUG_FOR_EUNIT, true).
-define(PCEP_CODEC_LOG_LEVEL, debug).

-ifdef(ENABLE_DEBUG).

-ifdef(DEBUG_FOR_EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG(FMT, ARGS), ?debugFmt(FMT, ARGS)).
-else.
-define(DEBUG(FMT, ARGS), io:format(FMT, ARGS)).
-endif.

-if(?PCEP_CODEC_LOG_LEVEL == debug).

-define(PCEP_DEBUG(CTX, FMT, ARGS),
        pcep_codec_context:log(CTX, debug, FMT, ARGS)).
-define(PCEP_DEBUG(CTX, FMT),
        pcep_codec_context:log(CTX, debug, FMT, [])).

-else.

-define(PCEP_DEBUG(CTX, FMT, ARGS), undefined).
-define(PCEP_DEBUG(CTX, FMT), undefined).

-endif.

-if(?PCEP_CODEC_LOG_LEVEL == info).
 or ?PCEP_CODEC_LOG_LEVEL == debug).

-define(PCEP_INFO(CTX, FMT, ARGS),
        pcep_codec_context:log(CTX, info, FMT, ARGS)).
-define(PCEP_INFO(CTX, FMT),
        pcep_codec_context:log(CTX, info, FMT, [])).

-else.

-define(PCEP_INFO(CTX, FMT, ARGS), undefined).
-define(PCEP_INFO(CTX, FMT), undefined).

-endif.


-if((?PCEP_CODEC_LOG_LEVEL == warning)
 or (?PCEP_CODEC_LOG_LEVEL == info)
 or (?PCEP_CODEC_LOG_LEVEL == debug)).

-define(PCEP_WARN(CTX, FMT, ARGS),
        pcep_codec_context:log(CTX, warning, FMT, ARGS)).
-define(PCEP_WARN(CTX, FMT),
        pcep_codec_context:log(CTX, warning, FMT, [])).

-else.

-define(PCEP_WARN(CTX, FMT, ARGS), undefined).
-define(PCEP_WARN(CTX, FMT), undefined).

-endif.


-if((?PCEP_CODEC_LOG_LEVEL == error)
 or (?PCEP_CODEC_LOG_LEVEL == warning)
 or (?PCEP_CODEC_LOG_LEVEL == info)
 or (?PCEP_CODEC_LOG_LEVEL == debug)).

-define(PCEP_ERROR(CTX, FMT, ARGS),
        pcep_codec_context:log(CTX, error, FMT, ARGS)).
-define(PCEP_ERROR(CTX, FMT),
        pcep_codec_context:log(CTX, error, FMT, [])).

-else.

-define(PCEP_ERROR(CTX, FMT, ARGS), undefined).
-define(PCEP_ERROR(CTX, FMT), undefined).

-endif.

-else.

-define(DEBUG(FMT, ARGS), undefined).
-define(PCEP_DEBUG(CTX, FMT, ARGS), undefined).
-define(PCEP_DEBUG(CTX, FMT), undefined).
-define(PCEP_INFO(CTX, FMT, ARGS), undefined).
-define(PCEP_INFO(CTX, FMT), undefined).
-define(PCEP_WARN(CTX, FMT, ARGS), undefined).
-define(PCEP_WARN(CTX, FMT), undefined).
-define(PCEP_ERROR(CTX, FMT, ARGS), undefined).
-define(PCEP_ERROR(CTX, FMT), undefined).

-endif.


%%% INTERNAL TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type pcep_message_type() :: open
                           | keepalive
                           | compreq
                           | comprep
                           | notif
                           | error
                           | close
                           | report
                           | update
                           | initiate.

-type pcep_object_class() :: open
                           | rp
                           | nopath
                           | endpoint
                           | bandwidth
                           | metric
                           | ero
                           | rro
                           | lspa
                           | iro
                           | svec
                           | notif
                           | error
                           | load_balancing
                           | close
                           | objfun
                           | lsp
                           | srp
                           | vendor_info
                           | inter_layer
                           | switch_layer
                           | req_adap_cap
                           | server_indication
                           | association.

-type pcep_object_type() :: open
                          | rp
                          | nopath
                          | endpoint_ipv4_addr
                          | endpoint_ipv6_addr
                          | bandwidth_req
                          | bandwidth_telsp
                          | bandwidth_cisco
                          | metric
                          | ero
                          | rro
                          | lspa
                          | iro
                          | svec
                          | notif
                          | error
                          | load_balancing
                          | close
                          | objfun
                          | lsp
                          | srp
                          | vendor_info
                          | inter_layer
                          | switch_layer
                          | req_adap_cap
                          | server_indication
                          | association_ipv4
                          | association_ipv6.

-type pcep_tlv_type() :: unknown_tlv
                       | nopath_vector
                       | overload_duration
                       | req_missing
                       | objfun_list
                       | vendor_info
                       | stateful_pce_cap
                       | symbolic_path_name
                       | ipv4_lsp_id
                       | ipv6_lsp_id
                       | lsp_error_code
                       | rsvp_error_spec
                       | lsp_db_version
                       | speaker_entity_id
                       | sr_pce_cap
                       | path_setup_type
                       | assoc_range
                       | global_assoc_source
                       | ext_assoc_id
                       | path_setup_type_cap
                       | assoc_type_list.

-type pcep_sub_tlv_type() :: unknown_subtlv
                           | path_setup_type_cap_sr.

-type pcep_ro_subobj_type() :: ipv4 | ipv6 | label | unum | asn | sr.
