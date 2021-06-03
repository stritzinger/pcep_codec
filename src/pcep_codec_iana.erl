-module(pcep_codec_iana).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").
-include("pcep_codec_iana.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([int_message_type/1]).
-export([pcep_message_type/1]).
-export([int_object_class_type/2]).
-export([pcep_object_class_type/2]).
-export([int_nopath_nature/1]).
-export([pcep_nopath_nature/1]).
-export([int_metric_type/1]).
-export([pcep_metric_type/1]).
-export([int_ro_subobj_type/1]).
-export([pcep_ro_subobj_type/1]).
-export([int_tlv_type/1]).
-export([pcep_tlv_type/1]).
-export([int_sub_tlv_type/2]).
-export([pcep_sub_tlv_type/1]).
-export([int_notif/2]).
-export([pcep_notif/2]).
-export([int_error/2]).
-export([pcep_error/1, pcep_error/2]).
-export([int_close_reason/1]).
-export([pcep_close_reason/1]).
-export([int_lsp_error/1]).
-export([pcep_lsp_error/1]).
-export([int_pst/1]).
-export([pcep_pst/1]).
-export([int_nai_type/1]).
-export([pcep_nai_type/1]).
-export([int_opstatus/1]).
-export([pcep_opstatus/1]).
-export([error_type_from_value/1]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec int_message_type(non_neg_integer()) -> pcep_message_type() | undefined.
int_message_type(?PCEP_MSG_OPEN) -> open;
int_message_type(?PCEP_MSG_KEEPALIVE) -> keepalive;
int_message_type(?PCEP_MSG_COMP_REQ) -> compreq;
int_message_type(?PCEP_MSG_COMP_REP) -> comprep;
int_message_type(?PCEP_MSG_NOTIFICATION) -> notif;
int_message_type(?PCEP_MSG_ERROR) -> error;
int_message_type(?PCEP_MSG_CLOSE) -> close;
int_message_type(?PCEP_MSG_REPORT) -> report;
int_message_type(?PCEP_MSG_UPDATE) -> update;
int_message_type(?PCEP_MSG_INITIATE) -> initiate;
int_message_type(_TypeId) -> undefined.

-spec pcep_message_type(pcep_message_type()) -> non_neg_integer().
pcep_message_type(open) -> ?PCEP_MSG_OPEN;
pcep_message_type(keepalive) -> ?PCEP_MSG_KEEPALIVE;
pcep_message_type(compreq) -> ?PCEP_MSG_COMP_REQ;
pcep_message_type(comprep) -> ?PCEP_MSG_COMP_REP;
pcep_message_type(notif) -> ?PCEP_MSG_NOTIFICATION;
pcep_message_type(error) -> ?PCEP_MSG_ERROR;
pcep_message_type(close) -> ?PCEP_MSG_CLOSE;
pcep_message_type(report) -> ?PCEP_MSG_REPORT;
pcep_message_type(update) -> ?PCEP_MSG_UPDATE;
pcep_message_type(initiate) -> ?PCEP_MSG_INITIATE.

-spec int_object_class_type(non_neg_integer(), non_neg_integer()) ->
    {pcep_object_class() | undefined, pcep_object_type() | undefined}.
int_object_class_type(?PCEP_OBJC_OPEN, ?PCEP_OBJT_OPEN) ->
    {open, open};
int_object_class_type(?PCEP_OBJC_OPEN, _TypeId) ->
    {open, undefined};
int_object_class_type(?PCEP_OBJC_RP, ?PCEP_OBJT_RP) ->
    {rp, rp};
int_object_class_type(?PCEP_OBJC_RP, _TypeId) ->
    {rp, undefined};
int_object_class_type(?PCEP_OBJC_NOPATH, ?PCEP_OBJT_NOPATH) ->
    {nopath, nopath};
int_object_class_type(?PCEP_OBJC_NOPATH, _TypeId) ->
    {nopath, undefined};
int_object_class_type(?PCEP_OBJC_ENDPOINT, ?PCEP_OBJT_ENDPOINT_IPV4_ADDR) ->
    {endpoint, endpoint_ipv4_addr};
int_object_class_type(?PCEP_OBJC_ENDPOINT, ?PCEP_OBJT_ENDPOINT_IPV6_ADDR) ->
    {endpoint, endpoint_ipv6_addr};
int_object_class_type(?PCEP_OBJC_ENDPOINT, _TypeId) ->
    {endpoint, undefined};
int_object_class_type(?PCEP_OBJC_BANDWIDTH, ?PCEP_OBJT_BANDWIDTH_REQ) ->
    {bandwidth, bandwidth_req};
int_object_class_type(?PCEP_OBJC_BANDWIDTH, ?PCEP_OBJT_BANDWIDTH_TELSP) ->
    {bandwidth, bandwidth_telsp};
int_object_class_type(?PCEP_OBJC_BANDWIDTH, ?PCEP_OBJT_BANDWIDTH_CISCO) ->
    {bandwidth, bandwidth_cisco};
int_object_class_type(?PCEP_OBJC_BANDWIDTH, _TypeId) ->
    {bandwidth, undefined};
int_object_class_type(?PCEP_OBJC_METRIC, ?PCEP_OBJT_METRIC) ->
    {metric, metric};
int_object_class_type(?PCEP_OBJC_METRIC, _TypeId) ->
    {metric, undefined};
int_object_class_type(?PCEP_OBJC_ERO, ?PCEP_OBJT_ERO) ->
    {ero, ero};
int_object_class_type(?PCEP_OBJC_ERO, _TypeId) ->
    {ero, undefined};
int_object_class_type(?PCEP_OBJC_RRO, ?PCEP_OBJT_RRO) ->
    {rro, rro};
int_object_class_type(?PCEP_OBJC_RRO, _TypeId) ->
    {rro, undefined};
int_object_class_type(?PCEP_OBJC_LSPA, ?PCEP_OBJT_LSPA) ->
    {lspa, lspa};
int_object_class_type(?PCEP_OBJC_LSPA, _TypeId) ->
    {lspa, undefined};
int_object_class_type(?PCEP_OBJC_IRO, ?PCEP_OBJT_IRO) ->
    {iro, iro};
int_object_class_type(?PCEP_OBJC_IRO, _TypeId) ->
    {iro, undefined};
int_object_class_type(?PCEP_OBJC_SVEC, ?PCEP_OBJT_SVEC) ->
    {svec, svec};
int_object_class_type(?PCEP_OBJC_SVEC, _TypeId) ->
    {svec, undefined};
int_object_class_type(?PCEP_OBJC_NOTIF, ?PCEP_OBJT_NOTIF) ->
    {notif, notif};
int_object_class_type(?PCEP_OBJC_NOTIF, _TypeId) ->
    {notif, undefined};
int_object_class_type(?PCEP_OBJC_ERROR, ?PCEP_OBJT_ERROR) ->
    {error, error};
int_object_class_type(?PCEP_OBJC_ERROR, _TypeId) ->
    {error, undefined};
int_object_class_type(?PCEP_OBJC_LOAD_BALANCING, ?PCEP_OBJT_LOAD_BALANCING) ->
    {load_balancing, load_balancing};
int_object_class_type(?PCEP_OBJC_LOAD_BALANCING, _TypeId) ->
    {load_balancing, undefined};
int_object_class_type(?PCEP_OBJC_CLOSE, ?PCEP_OBJT_CLOSE) ->
    {close, close};
int_object_class_type(?PCEP_OBJC_CLOSE, _TypeId) ->
    {close, undefined};
int_object_class_type(?PCEP_OBJC_OF, ?PCEP_OBJT_OF) ->
    {objfun, objfun};
int_object_class_type(?PCEP_OBJC_OF, _TypeId) ->
    {objfun, undefined};
int_object_class_type(?PCEP_OBJC_LSP, ?PCEP_OBJT_LSP) ->
    {lsp, lsp};
int_object_class_type(?PCEP_OBJC_LSP, _TypeId) ->
    {lsp, undefined};
int_object_class_type(?PCEP_OBJC_SRP, ?PCEP_OBJT_SRP) ->
    {srp, srp};
int_object_class_type(?PCEP_OBJC_SRP, _TypeId) ->
    {srp, undefined};
int_object_class_type(?PCEP_OBJC_VENDOR_INFO, ?PCEP_OBJT_VENDOR_INFO) ->
    {vendor_info, vendor_info};
int_object_class_type(?PCEP_OBJC_VENDOR_INFO, _TypeId) ->
    {vendor_info, undefined};
int_object_class_type(?PCEP_OBJC_INTER_LAYER, ?PCEP_OBJT_INTER_LAYER) ->
    {inter_layer, inter_layer};
int_object_class_type(?PCEP_OBJC_INTER_LAYER, _TypeId) ->
    {inter_layer, undefined};
int_object_class_type(?PCEP_OBJC_SWITCH_LAYER, ?PCEP_OBJT_SWITCH_LAYER) ->
    {switch_layer, switch_layer};
int_object_class_type(?PCEP_OBJC_SWITCH_LAYER, _TypeId) ->
    {switch_layer, undefined};
int_object_class_type(?PCEP_OBJC_REQ_ADAP_CAP, ?PCEP_OBJT_REQ_ADAP_CAP) ->
    {req_adap_cap, req_adap_cap};
int_object_class_type(?PCEP_OBJC_REQ_ADAP_CAP, _TypeId) ->
    {req_adap_cap, undefined};
int_object_class_type(?PCEP_OBJC_SERVER_INDICATION, ?PCEP_OBJT_SERVER_INDICATION) ->
    {server_indication, server_indication};
int_object_class_type(?PCEP_OBJC_SERVER_INDICATION, _TypeId) ->
    {server_indication, undefined};
int_object_class_type(?PCEP_OBJC_ASSOCIATION, ?PCEP_OBJT_ASSOCIATION_IPV4) ->
    {association, association_ipv4};
int_object_class_type(?PCEP_OBJC_ASSOCIATION, ?PCEP_OBJT_ASSOCIATION_IPV6) ->
    {association, association_ipv6};
int_object_class_type(?PCEP_OBJC_ASSOCIATION, _TypeId) ->
    {association, undefined};
int_object_class_type(_ClassId, _TypeId) ->
    {undefined, undefined}.


-spec pcep_object_class_type(pcep_object_class(), pcep_object_type()) ->
    {non_neg_integer(), non_neg_integer()}.
pcep_object_class_type(open, open) ->
    {?PCEP_OBJC_OPEN, ?PCEP_OBJT_OPEN};
pcep_object_class_type(rp, rp) ->
    {?PCEP_OBJC_RP, ?PCEP_OBJT_RP};
pcep_object_class_type(nopath, nopath) ->
    {?PCEP_OBJC_NOPATH, ?PCEP_OBJT_NOPATH};
pcep_object_class_type(endpoint, endpoint_ipv4_addr) ->
    {?PCEP_OBJC_ENDPOINT, ?PCEP_OBJT_ENDPOINT_IPV4_ADDR};
pcep_object_class_type(endpoint, endpoint_ipv6_addr) ->
    {?PCEP_OBJC_ENDPOINT, ?PCEP_OBJT_ENDPOINT_IPV6_ADDR};
pcep_object_class_type(bandwidth, bandwidth_req) ->
    {?PCEP_OBJC_BANDWIDTH, ?PCEP_OBJT_BANDWIDTH_REQ};
pcep_object_class_type(bandwidth, bandwidth_telsp) ->
    {?PCEP_OBJC_BANDWIDTH, ?PCEP_OBJT_BANDWIDTH_TELSP};
pcep_object_class_type(bandwidth, bandwidth_cisco) ->
    {?PCEP_OBJC_BANDWIDTH, ?PCEP_OBJT_BANDWIDTH_CISCO};
pcep_object_class_type(metric, metric) ->
    {?PCEP_OBJC_METRIC, ?PCEP_OBJT_METRIC};
pcep_object_class_type(ero, ero) ->
    {?PCEP_OBJC_ERO, ?PCEP_OBJT_ERO};
pcep_object_class_type(rro, rro) ->
    {?PCEP_OBJC_RRO, ?PCEP_OBJT_RRO};
pcep_object_class_type(lspa, lspa) ->
    {?PCEP_OBJC_LSPA, ?PCEP_OBJT_LSPA};
pcep_object_class_type(iro, iro) ->
    {?PCEP_OBJC_IRO, ?PCEP_OBJT_IRO};
pcep_object_class_type(svec, svec) ->
    {?PCEP_OBJC_SVEC, ?PCEP_OBJT_SVEC};
pcep_object_class_type(notif, notif) ->
    {?PCEP_OBJC_NOTIF, ?PCEP_OBJT_NOTIF};
pcep_object_class_type(error, error) ->
    {?PCEP_OBJC_ERROR, ?PCEP_OBJT_ERROR};
pcep_object_class_type(load_balancing, load_balancing) ->
    {?PCEP_OBJC_LOAD_BALANCING, ?PCEP_OBJT_LOAD_BALANCING};
pcep_object_class_type(close, close) ->
    {?PCEP_OBJC_CLOSE, ?PCEP_OBJT_CLOSE};
pcep_object_class_type(objfun, objfun) ->
    {?PCEP_OBJC_OF, ?PCEP_OBJT_OF};
pcep_object_class_type(lsp, lsp) ->
    {?PCEP_OBJC_LSP, ?PCEP_OBJT_LSP};
pcep_object_class_type(srp, srp) ->
    {?PCEP_OBJC_SRP, ?PCEP_OBJT_SRP};
pcep_object_class_type(vendor_info, vendor_info) ->
    {?PCEP_OBJC_VENDOR_INFO, ?PCEP_OBJT_VENDOR_INFO};
pcep_object_class_type(inter_layer, inter_layer) ->
    {?PCEP_OBJC_INTER_LAYER, ?PCEP_OBJT_INTER_LAYER};
pcep_object_class_type(switch_layer, switch_layer) ->
    {?PCEP_OBJC_SWITCH_LAYER, ?PCEP_OBJT_SWITCH_LAYER};
pcep_object_class_type(req_adap_cap, req_adap_cap) ->
    {?PCEP_OBJC_REQ_ADAP_CAP, ?PCEP_OBJT_REQ_ADAP_CAP};
pcep_object_class_type(server_indication, server_indication) ->
    {?PCEP_OBJC_SERVER_INDICATION, ?PCEP_OBJT_SERVER_INDICATION};
pcep_object_class_type(association, association_ipv4) ->
    {?PCEP_OBJC_ASSOCIATION, ?PCEP_OBJT_ASSOCIATION_IPV4};
pcep_object_class_type(association, association_ipv6) ->
    {?PCEP_OBJC_ASSOCIATION, ?PCEP_OBJT_ASSOCIATION_IPV6}.

-spec int_nopath_nature(non_neg_integer()) -> pcep_nopath_nature() | undefined.
int_nopath_nature(?PCEP_NOPATH_NI_PATH_NOT_FOUND) -> path_not_found;
int_nopath_nature(?PCEP_NOPATH_NI_PCE_CHAIN_BROKEN) -> pce_chain_broken;
int_nopath_nature(_Other) -> undefined.

-spec pcep_nopath_nature(pcep_nopath_nature()) -> non_neg_integer().
pcep_nopath_nature(path_not_found) -> ?PCEP_NOPATH_NI_PATH_NOT_FOUND;
pcep_nopath_nature(pce_chain_broken) -> ?PCEP_NOPATH_NI_PCE_CHAIN_BROKEN.

-spec int_metric_type(non_neg_integer()) -> te_metric_type() | undefined.
int_metric_type(?PCEP_METRIC_IGP) -> igp;
int_metric_type(?PCEP_METRIC_TE) -> te;
int_metric_type(?PCEP_METRIC_HOP_COUNT) -> hop_count;
int_metric_type(?PCEP_METRIC_AGGREGATE_BW) -> aggregate_bw;
int_metric_type(?PCEP_METRIC_MOST_LOADED_LINK) -> most_loaded_link;
int_metric_type(?PCEP_METRIC_CUMULATIVE_IGP) -> cumulative_igp;
int_metric_type(?PCEP_METRIC_CUMULATIVE_TE) -> cumulative_te;
int_metric_type(?PCEP_METRIC_SEGMENT_ID_DEPTH) -> segment_id_depth;
int_metric_type(_Other) -> undefined.

-spec pcep_metric_type(te_metric_type()) -> non_neg_integer().
pcep_metric_type(igp) -> ?PCEP_METRIC_IGP;
pcep_metric_type(te) -> ?PCEP_METRIC_TE;
pcep_metric_type(hop_count) -> ?PCEP_METRIC_HOP_COUNT;
pcep_metric_type(aggregate_bw) -> ?PCEP_METRIC_AGGREGATE_BW;
pcep_metric_type(most_loaded_link) -> ?PCEP_METRIC_MOST_LOADED_LINK;
pcep_metric_type(cumulative_igp) -> ?PCEP_METRIC_CUMULATIVE_IGP;
pcep_metric_type(cumulative_te) -> ?PCEP_METRIC_CUMULATIVE_TE;
pcep_metric_type(segment_id_depth) -> ?PCEP_METRIC_SEGMENT_ID_DEPTH.

-spec int_ro_subobj_type(non_neg_integer()) ->
    pcep_ro_subobj_type() | undefined.
int_ro_subobj_type(?RSVP_ERO_SUBOBJ_TYPE_IPV4) -> ipv4;
int_ro_subobj_type(?RSVP_ERO_SUBOBJ_TYPE_IPV6) -> ipv6;
int_ro_subobj_type(?RSVP_ERO_SUBOBJ_TYPE_LABEL) -> label;
int_ro_subobj_type(?RSVP_ERO_SUBOBJ_TYPE_UNUM) -> unum;
int_ro_subobj_type(?RSVP_ERO_SUBOBJ_TYPE_ASN) -> asn;
int_ro_subobj_type(?RSVP_ERO_SUBOBJ_TYPE_SR) -> sr;
int_ro_subobj_type(_Other) -> undefined.

-spec pcep_ro_subobj_type(pcep_ro_subobj_type()) -> non_neg_integer().
pcep_ro_subobj_type(ipv4) -> ?RSVP_ERO_SUBOBJ_TYPE_IPV4;
pcep_ro_subobj_type(ipv6) -> ?RSVP_ERO_SUBOBJ_TYPE_IPV6;
pcep_ro_subobj_type(label) -> ?RSVP_ERO_SUBOBJ_TYPE_LABEL;
pcep_ro_subobj_type(unum) -> ?RSVP_ERO_SUBOBJ_TYPE_UNUM;
pcep_ro_subobj_type(asn) -> ?RSVP_ERO_SUBOBJ_TYPE_ASN;
pcep_ro_subobj_type(sr) -> ?RSVP_ERO_SUBOBJ_TYPE_SR.

-spec int_tlv_type(non_neg_integer()) -> pcep_tlv_type() | undefined.
int_tlv_type(?PCEP_TLV_NOPATH_VECTOR) -> nopath_vector;
int_tlv_type(?PCEP_TLV_OVERLOAD_DURATION) -> overload_duration;
int_tlv_type(?PCEP_TLV_REQ_MISSING) -> req_missing;
int_tlv_type(?PCEP_TLV_OBJFUN_LIST) -> objfun_list;
int_tlv_type(?PCEP_TLV_VENDOR_INFO) -> vendor_info;
int_tlv_type(?PCEP_TLV_STATEFUL_PCE_CAP) -> stateful_pce_cap;
int_tlv_type(?PCEP_TLV_SYMBOLIC_PATH_NAME) -> symbolic_path_name;
int_tlv_type(?PCEP_TLV_IPV4_LSP_ID) -> ipv4_lsp_id;
int_tlv_type(?PCEP_TLV_IPV6_LSP_ID) -> ipv6_lsp_id;
int_tlv_type(?PCEP_TLV_LSP_ERROR_CODE) -> lsp_error_code;
int_tlv_type(?PCEP_TLV_RSVP_ERROR_SPEC) -> rsvp_error_spec;
int_tlv_type(?PCEP_TLV_LSP_DB_VERSION) -> lsp_db_version;
int_tlv_type(?PCEP_TLV_SPEAKER_ENTITY_ID) -> speaker_entity_id;
int_tlv_type(?PCEP_TLV_SR_PCE_CAP) -> sr_pce_cap;
int_tlv_type(?PCEP_TLV_PATH_SETUP_TYPE) -> path_setup_type;
int_tlv_type(?PCEP_TLV_OP_CONFIGURED_ASSOC_RANGE) -> assoc_range;
int_tlv_type(?PCEP_TLV_GLOBAL_ASSOC_SOURCE) -> global_assoc_source;
int_tlv_type(?PCEP_TLV_EXT_ASSOC_ID) -> ext_assoc_id;
int_tlv_type(?PCEP_TLV_PATH_SETUP_TYPE_CAP) -> path_setup_type_cap;
int_tlv_type(?PCEP_TLV_ASSOC_TYPE_LIST) -> assoc_type_list;
int_tlv_type(_TlvTypeId) -> undefined.

-spec pcep_tlv_type(pcep_tlv_type()) -> non_neg_integer().
pcep_tlv_type(nopath_vector) -> ?PCEP_TLV_NOPATH_VECTOR;
pcep_tlv_type(overload_duration) -> ?PCEP_TLV_OVERLOAD_DURATION;
pcep_tlv_type(req_missing) -> ?PCEP_TLV_REQ_MISSING;
pcep_tlv_type(objfun_list) -> ?PCEP_TLV_OBJFUN_LIST;
pcep_tlv_type(vendor_info) -> ?PCEP_TLV_VENDOR_INFO;
pcep_tlv_type(stateful_pce_cap) -> ?PCEP_TLV_STATEFUL_PCE_CAP;
pcep_tlv_type(symbolic_path_name) -> ?PCEP_TLV_SYMBOLIC_PATH_NAME;
pcep_tlv_type(ipv4_lsp_id) -> ?PCEP_TLV_IPV4_LSP_ID;
pcep_tlv_type(ipv6_lsp_id) -> ?PCEP_TLV_IPV6_LSP_ID;
pcep_tlv_type(lsp_error_code) -> ?PCEP_TLV_LSP_ERROR_CODE;
pcep_tlv_type(rsvp_error_spec) -> ?PCEP_TLV_RSVP_ERROR_SPEC;
pcep_tlv_type(lsp_db_version) -> ?PCEP_TLV_LSP_DB_VERSION;
pcep_tlv_type(speaker_entity_id) -> ?PCEP_TLV_SPEAKER_ENTITY_ID;
pcep_tlv_type(sr_pce_cap) -> ?PCEP_TLV_SR_PCE_CAP;
pcep_tlv_type(path_setup_type) -> ?PCEP_TLV_PATH_SETUP_TYPE;
pcep_tlv_type(assoc_range) -> ?PCEP_TLV_OP_CONFIGURED_ASSOC_RANGE;
pcep_tlv_type(global_assoc_source) -> ?PCEP_TLV_GLOBAL_ASSOC_SOURCE;
pcep_tlv_type(ext_assoc_id) -> ?PCEP_TLV_EXT_ASSOC_ID;
pcep_tlv_type(path_setup_type_cap) -> ?PCEP_TLV_PATH_SETUP_TYPE_CAP;
pcep_tlv_type(assoc_type_list) -> ?PCEP_TLV_ASSOC_TYPE_LIST.

-spec int_sub_tlv_type(pcep_tlv_type(), non_neg_integer())
    -> pcep_sub_tlv_type() | undefined.
int_sub_tlv_type(path_setup_type_cap, ?PCEP_TLV_PATH_SETUP_TYPE_CAP_SR) ->
    path_setup_type_cap_sr;
int_sub_tlv_type(_OtherTLV, _OtherValue) ->
    undefined.

-spec pcep_sub_tlv_type(pcep_sub_tlv_type()) -> non_neg_integer().
pcep_sub_tlv_type(path_setup_type_cap_sr) ->
    ?PCEP_TLV_PATH_SETUP_TYPE_CAP_SR.

-spec int_notif(non_neg_integer(), non_neg_integer())
    -> {pcep_notif_type() | undefined, pcep_notif_value() | undefined}.
int_notif(?PCEP_NOTT_REQ_CANCELED, ?PCEP_NOTV_PCC_CANCELED_REQ) ->
    {req_canceled, pcc_canceled_req};
int_notif(?PCEP_NOTT_REQ_CANCELED, ?PCEP_NOTV_PCE_CANCELED_REQ) ->
    {req_canceled, pce_canceled_req};
int_notif(?PCEP_NOTT_REQ_CANCELED, _OtherValue) ->
    {req_canceled, undefined};
int_notif(?PCEP_NOTT_PCE_CONGESTION, ?PCEP_NOTV_PCE_CONGESTED) ->
    {pce_congestion, pce_congested};
int_notif(?PCEP_NOTT_PCE_CONGESTION, ?PCEP_NOTV_PCE_RECOVERED) ->
    {pce_congestion, pce_recovered};
int_notif(?PCEP_NOTT_PCE_CONGESTION, _OtherValue) ->
    {pce_congestion, undefined};
int_notif(?PCEP_NOTT_PCE_LIMIT_EXCEEDED, ?PCEP_NOTV_PCE_LIMIT_EXCEEDED) ->
    {pce_limit_exceeded, pce_limit_exceeded};
int_notif(?PCEP_NOTT_PCE_LIMIT_EXCEEDED, _OtherValue) ->
    {pce_limit_exceeded, undefined};
int_notif(_OtherType, _OtherValue) ->
    {undefined, undefined}.

-spec pcep_notif(pcep_notif_type(), pcep_notif_value())
    -> {non_neg_integer(), non_neg_integer()}.
pcep_notif(req_canceled, pcc_canceled_req) ->
    {?PCEP_NOTT_REQ_CANCELED, ?PCEP_NOTV_PCC_CANCELED_REQ};
pcep_notif(req_canceled, pce_canceled_req) ->
    {?PCEP_NOTT_REQ_CANCELED, ?PCEP_NOTV_PCE_CANCELED_REQ};
pcep_notif(pce_congestion, pce_congested) ->
    {?PCEP_NOTT_PCE_CONGESTION, ?PCEP_NOTV_PCE_CONGESTED};
pcep_notif(pce_congestion, pce_recovered) ->
    {?PCEP_NOTT_PCE_CONGESTION, ?PCEP_NOTV_PCE_RECOVERED};
pcep_notif(pce_limit_exceeded, pce_limit_exceeded) ->
    {?PCEP_NOTT_PCE_LIMIT_EXCEEDED, ?PCEP_NOTV_PCE_LIMIT_EXCEEDED}.

-spec int_error(non_neg_integer(), non_neg_integer())
    -> {pcep_error_type() | undefined, pcep_error_value() | undefined}.
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_RECVD_INVALID_OPEN_MSG) ->
    {session_failure, invalid_open_message};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_OPENWAIT_TIMED_OUT) ->
    {session_failure, openwait_timed_out};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NO_NEG) ->
    {session_failure, unacceptable_open_msg_no_neg};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NEG) ->
    {session_failure, unacceptable_open_msg_neg};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_RECVD_SECOND_OPEN_MSG_UNACCEPTABLE) ->
    {session_failure, second_open_msg_unacceptable};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_RECVD_PCERR) ->
    {session_failure, recvd_pcerr};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_KEEPALIVEWAIT_TIMED_OUT) ->
    {session_failure, keepalivewait_timed_out};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_PCEP_VERSION_NOT_SUPPORTED) ->
    {session_failure, pcep_version_not_supported};
int_error(?PCEP_ERRT_SESSION_FAILURE,
          ?PCEP_ERRV_UNASSIGNED) ->
    {session_failure, session_failure};
int_error(?PCEP_ERRT_SESSION_FAILURE, _OtherValue) ->
    {session_failure, undefined};
int_error(?PCEP_ERRT_CAPABILITY_NOT_SUPPORTED,
          ?PCEP_ERRV_UNASSIGNED) ->
    {capability_not_supported, capability_not_supported};
int_error(?PCEP_ERRT_CAPABILITY_NOT_SUPPORTED, _OtherValue) ->
    {capability_not_supported, undefined};
int_error(?PCEP_ERRT_UNKNOW_OBJECT,
          ?PCEP_ERRV_UNREC_OBJECT_CLASS) ->
    {unknown_object, unrec_object_class};
int_error(?PCEP_ERRT_UNKNOW_OBJECT,
          ?PCEP_ERRV_UNREC_OBJECT_TYPE) ->
    {unknown_object, unrec_object_type};
int_error(?PCEP_ERRT_UNKNOW_OBJECT,
          ?PCEP_ERRV_UNASSIGNED) ->
    {unknown_object, unknown_object};
int_error(?PCEP_ERRT_UNKNOW_OBJECT, _OtherValue) ->
    {unknown_object, undefined};
int_error(?PCEP_ERRT_NOT_SUPPORTED_OBJECT,
          ?PCEP_ERRV_NOT_SUPPORTED_OBJECT_CLASS) ->
    {not_supported_object, not_supported_object_class};
int_error(?PCEP_ERRT_NOT_SUPPORTED_OBJECT,
          ?PCEP_ERRV_NOT_SUPPORTED_OBJECT_TYPE) ->
    {not_supported_object, not_supported_object_type};
int_error(?PCEP_ERRT_NOT_SUPPORTED_OBJECT,
          ?PCEP_ERRV_UNASSIGNED) ->
    {not_supported_object, not_supported_object};
int_error(?PCEP_ERRT_NOT_SUPPORTED_OBJECT, _OtherValue) ->
    {not_supported_object, undefined};
int_error(?PCEP_ERRT_POLICY_VIOLATION,
          ?PCEP_ERRV_C_BIT_SET_IN_METRIC_OBJECT) ->
    {policy_violation, c_bit_set_in_metric_object};
int_error(?PCEP_ERRT_POLICY_VIOLATION,
          ?PCEP_ERRV_O_BIT_CLEARD_IN_RP_OBJECT) ->
    {policy_violation, o_bit_cleared_in_rp_object};
int_error(?PCEP_ERRT_POLICY_VIOLATION,
          ?PCEP_ERRV_OBJFUN_NOT_ALLOWED) ->
    {policy_violation, objfun_not_allowed};
int_error(?PCEP_ERRT_POLICY_VIOLATION,
          ?PCEP_ERRV_RP_OF_BIT_SET) ->
    {policy_violation, rp_of_bit_set};
int_error(?PCEP_ERRT_POLICY_VIOLATION,
          ?PCEP_ERRV_UNASSIGNED) ->
    {policy_violation, policy_violation};
int_error(?PCEP_ERRT_POLICY_VIOLATION, _OtherValue) ->
    {policy_violation, undefined};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_RP_OBJECT_MISSING) ->
    {mandatory_object_missing, rp_object_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_RRO_OBJECT_MISSING_FOR_REOP) ->
    {mandatory_object_missing, rro_object_missing_for_reop};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_EP_OBJECT_MISSING) ->
    {mandatory_object_missing, ep_object_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_LSP_OBJECT_MISSING) ->
    {mandatory_object_missing, lsp_object_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_ERO_OBJECT_MISSING) ->
    {mandatory_object_missing, ero_object_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_SRP_OBJECT_MISSING) ->
    {mandatory_object_missing, srp_object_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_LSP_ID_TLV_MISSING) ->
    {mandatory_object_missing, lsp_id_tlv_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_LSP_DB_TLV_MISSING) ->
    {mandatory_object_missing, lsp_db_tlv_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
          ?PCEP_ERRV_UNASSIGNED) ->
    {mandatory_object_missing, mandatory_object_missing};
int_error(?PCEP_ERRT_MANDATORY_OBJECT_MISSING, _OtherValue) ->
    {mandatory_object_missing, undefined};
int_error(?PCEP_ERRT_SYNC_PC_REQ_MISSING,
          ?PCEP_ERRV_UNASSIGNED) ->
    {sync_pc_req_missing, sync_pc_req_missing};
int_error(?PCEP_ERRT_SYNC_PC_REQ_MISSING, _OtherValue) ->
    {sync_pc_req_missing, undefined};
int_error(?PCEP_ERRT_UNKNOWN_REQ_REF,
          ?PCEP_ERRV_UNASSIGNED) ->
    {unknown_req_ref, unknown_req_ref};
int_error(?PCEP_ERRT_UNKNOWN_REQ_REF, _OtherValue) ->
    {unknown_req_ref, undefined};
int_error(?PCEP_ERRT_SECOND_PCEP_SESSION_ATTEMPTED,
          ?PCEP_ERRV_UNASSIGNED) ->
    {second_pcep_session_attempted, second_pcep_session_attempted};
int_error(?PCEP_ERRT_SECOND_PCEP_SESSION_ATTEMPTED, _OtherValue) ->
    {second_pcep_session_attempted, undefined};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_P_FLAG_NOT_CORRECT_IN_OBJECT) ->
    {invalid_object, p_flag_not_correct_in_object};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_BAD_LABEL_VALUE) ->
    {invalid_object, bad_label_value};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_UNSUPPORTED_NUM_SR_ERO_SUBOBJECTS) ->
    {invalid_object, unsupported_num_sr_ero_subobjects};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_BAD_LABEL_FORMAT) ->
    {invalid_object, bad_label_format};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SR_ERO_MIXED) ->
    {invalid_object, sr_ero_mixed};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SR_ERO_SID_NAI_ABSENT) ->
    {invalid_object, sr_ero_sid_nai_absent};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SR_RRO_SID_NAI_ABSENT) ->
    {invalid_object, sr_rro_sid_nai_absent};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SYMBOLIC_PATH_NAME_MISSING) ->
    {invalid_object, symbolic_path_name_missing};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_MSD_EXCEEDS_DEFAULT) ->
    {invalid_object, msd_exceeds_default};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SR_RRO_MIXED) ->
    {invalid_object, sr_rro_mixed};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_MALFORMED_OBJECT) ->
    {invalid_object, malformed_object};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SR_CAP_TLV_MISSING) ->
    {invalid_object, sr_cap_tlv_missing};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_UNSUPPORTED_NAI_TYPE) ->
    {invalid_object, unsupported_nai_type};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_UNKNOWN_SID) ->
    {invalid_object, unknown_sid};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_CANNOT_RESOLVE_NAI) ->
    {invalid_object, cannot_resolve_nai};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SRGB_NOT_FOUND) ->
    {invalid_object, srgb_not_found};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SID_EXCEEDS_SRGB_SIZE) ->
    {invalid_object, sid_exceeds_srgb_size};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SRLB_NOT_FOUND) ->
    {invalid_object, srlb_not_found};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_SID_EXCEEDS_SRLB_SIZE) ->
    {invalid_object, sid_exceeds_srlb_size};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_INCONSISTENT_SIDS) ->
    {invalid_object, inconsistent_sids};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_MSD_MUST_BE_NONZERO) ->
    {invalid_object, msd_must_be_nonzero};
int_error(?PCEP_ERRT_INVALID_OBJECT,
          ?PCEP_ERRV_UNASSIGNED) ->
    {invalid_object, invalid_object};
int_error(?PCEP_ERRT_INVALID_OBJECT, _OtherValue) ->
    {invalid_object, undefined};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_LSP_UPDATE_FOR_NON_DELEGATED_LSP) ->
    {invalid_operation, lsp_update_for_non_delegated_lsp};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_LSP_UPDATE_NON_ADVERTISED_PCE) ->
    {invalid_operation, lsp_update_non_advertised_pce};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_LSP_UPDATE_UNKNOWN_PLSP_ID) ->
    {invalid_operation, lsp_update_unknown_plsp_id};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_LSP_REPORT_NON_ADVERTISED_PCE) ->
    {invalid_operation, lsp_report_non_advertised_pce};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_PCE_INIT_LSP_LIMIT_REACHED) ->
    {invalid_operation, pce_init_lsp_limit_reached};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_PCE_INIT_LSP_DELEGATION_CANT_REVOKE) ->
    {invalid_operation, pce_init_lsp_delegation_cant_revoke};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_LSP_INIT_NON_ZERO_PLSP_ID) ->
    {invalid_operation, lsp_init_non_zero_plsp_id};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_LSP_NOT_PCE_INITIATED) ->
    {invalid_operation, lsp_not_pce_initiated};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_PCE_INIT_OP_FREQ_LIMIT_REACHED) ->
    {invalid_operation, pce_init_op_freq_limit_reached};
int_error(?PCEP_ERRT_INVALID_OPERATION,
          ?PCEP_ERRV_UNASSIGNED) ->
    {invalid_operation, invalid_operation};
int_error(?PCEP_ERRT_INVALID_OPERATION, _OtherValue) ->
    {invalid_operation, undefined};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_PCE_CANT_PROCESS_LSP_REPORT) ->
    {lsp_state_sync_error, pce_cant_process_lsp_report};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_LSP_DB_VERSION_MISMATCH) ->
    {lsp_state_sync_error, lsp_db_version_mismatch};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_TRIGGER_ATTEMPT_BEFORE_PCE_TRIGGER) ->
    {lsp_state_sync_error, trigger_attempt_before_pce_trigger};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_TRIGGER_ATTEMPT_NO_PCE_TRIGGER_CAP) ->
    {lsp_state_sync_error, trigger_attempt_no_pce_trigger_cap};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_PCC_CANT_COMPLETE_STATE_SYNC) ->
    {lsp_state_sync_error, pcc_cant_complete_state_sync};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_INVALID_LSP_DB_VERSION_NUMBER) ->
    {lsp_state_sync_error, invalid_lsp_db_version_number};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_INVALID_SPEAKER_ENTITY_ID) ->
    {lsp_state_sync_error, invalid_speaker_entity_id};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
          ?PCEP_ERRV_UNASSIGNED) ->
    {lsp_state_sync_error, lsp_state_sync_error};
int_error(?PCEP_ERRT_LSP_STATE_SYNC_ERROR, _OtherValue) ->
    {lsp_state_sync_error, undefined};
int_error(?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE,
          ?PCEP_ERRV_UNSUPPORTED_PATH_SETUP_TYPE) ->
    {invalid_te_path_setup_type, unsupported_path_setup_type};
int_error(?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE,
          ?PCEP_ERRV_MISMATCHED_PATH_SETUP_TYPE) ->
    {invalid_te_path_setup_type, mismatched_path_setup_type};
int_error(?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE,
          ?PCEP_ERRV_UNASSIGNED) ->
    {invalid_te_path_setup_type, invalid_te_path_setup_type};
int_error(?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE, _OtherValue) ->
    {invalid_te_path_setup_type, undefined};
int_error(?PCEP_ERRT_BAD_PARAMETER_VALUE,
          ?PCEP_ERRV_SYMBOLIC_PATH_NAME_IN_USE) ->
    {bad_parameter_value, symbolic_path_name_in_use};
int_error(?PCEP_ERRT_BAD_PARAMETER_VALUE,
          ?PCEP_ERRV_LSP_SPEAKER_ID_NOT_PCE_INITIATED) ->
    {bad_parameter_value, lsp_speaker_id_not_pce_initiated};
int_error(?PCEP_ERRT_BAD_PARAMETER_VALUE,
          ?PCEP_ERRV_UNASSIGNED) ->
    {bad_parameter_value, bad_parameter_value};
int_error(?PCEP_ERRT_BAD_PARAMETER_VALUE, _OtherValue) ->
    {bad_parameter_value, undefined};
int_error(?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
          ?PCEP_ERRV_UNACCEPTABLE_INSTANTIATE_ERROR) ->
    {lsp_instantiate_error, unacceptable_instantiate_error};
int_error(?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
          ?PCEP_ERRV_INTERNAL_ERROR) ->
    {lsp_instantiate_error, internal_error};
int_error(?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
          ?PCEP_ERRV_SIGNALLING_ERROR) ->
    {lsp_instantiate_error, signalling_error};
int_error(?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
          ?PCEP_ERRV_UNASSIGNED) ->
    {lsp_instantiate_error, lsp_instantiate_error};
int_error(?PCEP_ERRT_LSP_INSTANTIATE_ERROR, _OtherValue) ->
    {lsp_instantiate_error, undefined};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_ASSOC_TYPE_NOT_SUPPORTED) ->
    {association_error, assoc_type_not_supported};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_TOO_MANY_LSPS_IN_ASSOC_GRP) ->
    {association_error, too_many_lsps_in_assoc_grp};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_TOO_MANY_ASSOC_GROUPS) ->
    {association_error, too_many_assoc_groups};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_ASSOCIATION_UNKNOWN) ->
    {association_error, association_unknown};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_OP_CONF_ASSOC_INFO_MISMATCH) ->
    {association_error, op_conf_assoc_info_mismatch};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_ASSOC_INFO_MISMATCH) ->
    {association_error, assoc_info_mismatch};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_CANNOT_JOIN_ASSOC_GROUP) ->
    {association_error, cannot_join_assoc_group};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_ASSOC_ID_NOT_IN_RANGE) ->
    {association_error, assoc_id_not_in_range};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR,
          ?PCEP_ERRV_UNASSIGNED) ->
    {association_error, association_error};
int_error(?PCEP_ERRT_ASSOCIATION_ERROR, _OtherValue) ->
    {association_error, undefined};
int_error(_OtherType, _OtherValue) ->
    {undefined, undefined}.

-spec pcep_error(pcep_error_type(), pcep_error_value())
    -> {non_neg_integer(), non_neg_integer()}.
pcep_error(session_failure, invalid_open_message) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_RECVD_INVALID_OPEN_MSG};
pcep_error(session_failure, openwait_timed_out) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_OPENWAIT_TIMED_OUT};
pcep_error(session_failure, unacceptable_open_msg_no_neg) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NO_NEG};
pcep_error(session_failure, unacceptable_open_msg_neg) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NEG};
pcep_error(session_failure, second_open_msg_unacceptable) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_RECVD_SECOND_OPEN_MSG_UNACCEPTABLE};
pcep_error(session_failure, recvd_pcerr) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_RECVD_PCERR};
pcep_error(session_failure, keepalivewait_timed_out) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_KEEPALIVEWAIT_TIMED_OUT};
pcep_error(session_failure, pcep_version_not_supported) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_PCEP_VERSION_NOT_SUPPORTED};
pcep_error(session_failure, session_failure) ->
    {?PCEP_ERRT_SESSION_FAILURE,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(capability_not_supported, capability_not_supported) ->
    {?PCEP_ERRT_CAPABILITY_NOT_SUPPORTED,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(unknown_object, unrec_object_class) ->
    {?PCEP_ERRT_UNKNOW_OBJECT,
     ?PCEP_ERRV_UNREC_OBJECT_CLASS};
pcep_error(unknown_object, unrec_object_type) ->
    {?PCEP_ERRT_UNKNOW_OBJECT,
     ?PCEP_ERRV_UNREC_OBJECT_TYPE};
pcep_error(unknown_object, unknown_object) ->
    {?PCEP_ERRT_UNKNOW_OBJECT,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(not_supported_object, not_supported_object_class) ->
    {?PCEP_ERRT_NOT_SUPPORTED_OBJECT,
     ?PCEP_ERRV_NOT_SUPPORTED_OBJECT_CLASS};
pcep_error(not_supported_object, not_supported_object_type) ->
    {?PCEP_ERRT_NOT_SUPPORTED_OBJECT,
     ?PCEP_ERRV_NOT_SUPPORTED_OBJECT_TYPE};
pcep_error(not_supported_object, not_supported_object) ->
    {?PCEP_ERRT_NOT_SUPPORTED_OBJECT,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(policy_violation, c_bit_set_in_metric_object) ->
    {?PCEP_ERRT_POLICY_VIOLATION,
     ?PCEP_ERRV_C_BIT_SET_IN_METRIC_OBJECT};
pcep_error(policy_violation, o_bit_cleared_in_rp_object) ->
    {?PCEP_ERRT_POLICY_VIOLATION,
     ?PCEP_ERRV_O_BIT_CLEARD_IN_RP_OBJECT};
pcep_error(policy_violation, objfun_not_allowed) ->
    {?PCEP_ERRT_POLICY_VIOLATION,
     ?PCEP_ERRV_OBJFUN_NOT_ALLOWED};
pcep_error(policy_violation, rp_of_bit_set) ->
    {?PCEP_ERRT_POLICY_VIOLATION,
     ?PCEP_ERRV_RP_OF_BIT_SET};
pcep_error(policy_violation, policy_violation) ->
    {?PCEP_ERRT_POLICY_VIOLATION,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(mandatory_object_missing, rp_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_RP_OBJECT_MISSING};
pcep_error(mandatory_object_missing, rro_object_missing_for_reop) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_RRO_OBJECT_MISSING_FOR_REOP};
pcep_error(mandatory_object_missing, ep_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_EP_OBJECT_MISSING};
pcep_error(mandatory_object_missing, lsp_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_LSP_OBJECT_MISSING};
pcep_error(mandatory_object_missing, ero_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_ERO_OBJECT_MISSING};
pcep_error(mandatory_object_missing, srp_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_SRP_OBJECT_MISSING};
pcep_error(mandatory_object_missing, lsp_id_tlv_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_LSP_ID_TLV_MISSING};
pcep_error(mandatory_object_missing, lsp_db_tlv_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_LSP_DB_TLV_MISSING};
pcep_error(mandatory_object_missing, mandatory_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(sync_pc_req_missing, sync_pc_req_missing) ->
    {?PCEP_ERRT_SYNC_PC_REQ_MISSING,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(unknown_req_ref, unknown_req_ref) ->
    {?PCEP_ERRT_UNKNOWN_REQ_REF,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(second_pcep_session_attempted, second_pcep_session_attempted) ->
    {?PCEP_ERRT_SECOND_PCEP_SESSION_ATTEMPTED,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(invalid_object, p_flag_not_correct_in_object) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_P_FLAG_NOT_CORRECT_IN_OBJECT};
pcep_error(invalid_object, bad_label_value) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_BAD_LABEL_VALUE};
pcep_error(invalid_object, unsupported_num_sr_ero_subobjects) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_UNSUPPORTED_NUM_SR_ERO_SUBOBJECTS};
pcep_error(invalid_object, bad_label_format) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_BAD_LABEL_FORMAT};
pcep_error(invalid_object, sr_ero_mixed) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SR_ERO_MIXED};
pcep_error(invalid_object, sr_ero_sid_nai_absent) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SR_ERO_SID_NAI_ABSENT};
pcep_error(invalid_object, sr_rro_sid_nai_absent) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SR_RRO_SID_NAI_ABSENT};
pcep_error(invalid_object, symbolic_path_name_missing) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SYMBOLIC_PATH_NAME_MISSING};
pcep_error(invalid_object, msd_exceeds_default) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_MSD_EXCEEDS_DEFAULT};
pcep_error(invalid_object, sr_rro_mixed) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SR_RRO_MIXED};
pcep_error(invalid_object, malformed_object) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_MALFORMED_OBJECT};
pcep_error(invalid_object, sr_cap_tlv_missing) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SR_CAP_TLV_MISSING};
pcep_error(invalid_object, unsupported_nai_type) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_UNSUPPORTED_NAI_TYPE};
pcep_error(invalid_object, unknown_sid) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_UNKNOWN_SID};
pcep_error(invalid_object, cannot_resolve_nai) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_CANNOT_RESOLVE_NAI};
pcep_error(invalid_object, srgb_not_found) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SRGB_NOT_FOUND};
pcep_error(invalid_object, sid_exceeds_srgb_size) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SID_EXCEEDS_SRGB_SIZE};
pcep_error(invalid_object, srlb_not_found) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SRLB_NOT_FOUND};
pcep_error(invalid_object, sid_exceeds_srlb_size) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_SID_EXCEEDS_SRLB_SIZE};
pcep_error(invalid_object, inconsistent_sids) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_INCONSISTENT_SIDS};
pcep_error(invalid_object, msd_must_be_nonzero) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_MSD_MUST_BE_NONZERO};
pcep_error(invalid_object, invalid_object) ->
    {?PCEP_ERRT_INVALID_OBJECT,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(invalid_operation, lsp_update_for_non_delegated_lsp) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_LSP_UPDATE_FOR_NON_DELEGATED_LSP};
pcep_error(invalid_operation, lsp_update_non_advertised_pce) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_LSP_UPDATE_NON_ADVERTISED_PCE};
pcep_error(invalid_operation, lsp_update_unknown_plsp_id) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_LSP_UPDATE_UNKNOWN_PLSP_ID};
pcep_error(invalid_operation, lsp_report_non_advertised_pce) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_LSP_REPORT_NON_ADVERTISED_PCE};
pcep_error(invalid_operation, pce_init_lsp_limit_reached) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_PCE_INIT_LSP_LIMIT_REACHED};
pcep_error(invalid_operation, pce_init_lsp_delegation_cant_revoke) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_PCE_INIT_LSP_DELEGATION_CANT_REVOKE};
pcep_error(invalid_operation, lsp_init_non_zero_plsp_id) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_LSP_INIT_NON_ZERO_PLSP_ID};
pcep_error(invalid_operation, lsp_not_pce_initiated) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_LSP_NOT_PCE_INITIATED};
pcep_error(invalid_operation, pce_init_op_freq_limit_reached) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_PCE_INIT_OP_FREQ_LIMIT_REACHED};
pcep_error(invalid_operation, invalid_operation) ->
    {?PCEP_ERRT_INVALID_OPERATION,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(lsp_state_sync_error, pce_cant_process_lsp_report) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_PCE_CANT_PROCESS_LSP_REPORT};
pcep_error(lsp_state_sync_error, lsp_db_version_mismatch) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_LSP_DB_VERSION_MISMATCH};
pcep_error(lsp_state_sync_error, trigger_attempt_before_pce_trigger) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_TRIGGER_ATTEMPT_BEFORE_PCE_TRIGGER};
pcep_error(lsp_state_sync_error, trigger_attempt_no_pce_trigger_cap) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_TRIGGER_ATTEMPT_NO_PCE_TRIGGER_CAP};
pcep_error(lsp_state_sync_error, pcc_cant_complete_state_sync) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_PCC_CANT_COMPLETE_STATE_SYNC};
pcep_error(lsp_state_sync_error, invalid_lsp_db_version_number) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_INVALID_LSP_DB_VERSION_NUMBER};
pcep_error(lsp_state_sync_error, invalid_speaker_entity_id) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_INVALID_SPEAKER_ENTITY_ID};
pcep_error(lsp_state_sync_error, lsp_state_sync_error) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR,
     ?PCEP_ERRV_UNASSIGNED};    
pcep_error(invalid_te_path_setup_type, unsupported_path_setup_type) ->
    {?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE,
     ?PCEP_ERRV_UNSUPPORTED_PATH_SETUP_TYPE};
pcep_error(invalid_te_path_setup_type, mismatched_path_setup_type) ->
    {?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE,
     ?PCEP_ERRV_MISMATCHED_PATH_SETUP_TYPE};
pcep_error(invalid_te_path_setup_type, invalid_te_path_setup_type) ->
    {?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(bad_parameter_value, symbolic_path_name_in_use) ->
    {?PCEP_ERRT_BAD_PARAMETER_VALUE,
     ?PCEP_ERRV_SYMBOLIC_PATH_NAME_IN_USE};
pcep_error(bad_parameter_value, lsp_speaker_id_not_pce_initiated) ->
    {?PCEP_ERRT_BAD_PARAMETER_VALUE,
     ?PCEP_ERRV_LSP_SPEAKER_ID_NOT_PCE_INITIATED};
pcep_error(bad_parameter_value, bad_parameter_value) ->
    {?PCEP_ERRT_BAD_PARAMETER_VALUE,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(lsp_instantiate_error, unacceptable_instantiate_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
     ?PCEP_ERRV_UNACCEPTABLE_INSTANTIATE_ERROR};
pcep_error(lsp_instantiate_error, internal_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
     ?PCEP_ERRV_INTERNAL_ERROR};
pcep_error(lsp_instantiate_error, signalling_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
     ?PCEP_ERRV_SIGNALLING_ERROR};
pcep_error(lsp_instantiate_error, lsp_instantiate_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR,
     ?PCEP_ERRV_UNASSIGNED};
pcep_error(association_error, assoc_type_not_supported) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_ASSOC_TYPE_NOT_SUPPORTED};
pcep_error(association_error, too_many_lsps_in_assoc_grp) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_TOO_MANY_LSPS_IN_ASSOC_GRP};
pcep_error(association_error, too_many_assoc_groups) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_TOO_MANY_ASSOC_GROUPS};
pcep_error(association_error, association_unknown) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_ASSOCIATION_UNKNOWN};
pcep_error(association_error, op_conf_assoc_info_mismatch) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_OP_CONF_ASSOC_INFO_MISMATCH};
pcep_error(association_error, assoc_info_mismatch) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_ASSOC_INFO_MISMATCH};
pcep_error(association_error, cannot_join_assoc_group) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_CANNOT_JOIN_ASSOC_GROUP};
pcep_error(association_error, assoc_id_not_in_range) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_ASSOC_ID_NOT_IN_RANGE};
pcep_error(association_error, association_error) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR,
     ?PCEP_ERRV_UNASSIGNED}.

-spec pcep_error(pcep_error_value())
    -> {non_neg_integer(), non_neg_integer()}.
pcep_error(invalid_open_message) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_RECVD_INVALID_OPEN_MSG};
pcep_error(openwait_timed_out) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_OPENWAIT_TIMED_OUT};
pcep_error(unacceptable_open_msg_no_neg) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NO_NEG};
pcep_error(unacceptable_open_msg_neg) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NEG};
pcep_error(second_open_msg_unacceptable) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_RECVD_SECOND_OPEN_MSG_UNACCEPTABLE};
pcep_error(recvd_pcerr) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_RECVD_PCERR};
pcep_error(keepalivewait_timed_out) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_KEEPALIVEWAIT_TIMED_OUT};
pcep_error(pcep_version_not_supported) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_PCEP_VERSION_NOT_SUPPORTED};
pcep_error(session_failure) ->
    {?PCEP_ERRT_SESSION_FAILURE, ?PCEP_ERRV_UNASSIGNED};
pcep_error(capability_not_supported) ->
    {?PCEP_ERRT_CAPABILITY_NOT_SUPPORTED, ?PCEP_ERRV_UNASSIGNED};
pcep_error(unrec_object_class) ->
    {?PCEP_ERRT_UNKNOW_OBJECT, ?PCEP_ERRV_UNREC_OBJECT_CLASS};
pcep_error(unrec_object_type) ->
    {?PCEP_ERRT_UNKNOW_OBJECT, ?PCEP_ERRV_UNREC_OBJECT_TYPE};
pcep_error(unknown_object) ->
    {?PCEP_ERRT_UNKNOW_OBJECT, ?PCEP_ERRV_UNASSIGNED};
pcep_error(not_supported_object_class) ->
    {?PCEP_ERRT_NOT_SUPPORTED_OBJECT, ?PCEP_ERRV_NOT_SUPPORTED_OBJECT_CLASS};
pcep_error(not_supported_object_type) ->
    {?PCEP_ERRT_NOT_SUPPORTED_OBJECT, ?PCEP_ERRV_NOT_SUPPORTED_OBJECT_TYPE};
pcep_error(not_supported_object) ->
    {?PCEP_ERRT_NOT_SUPPORTED_OBJECT, ?PCEP_ERRV_UNASSIGNED};
pcep_error(c_bit_set_in_metric_object) ->
    {?PCEP_ERRT_POLICY_VIOLATION, ?PCEP_ERRV_C_BIT_SET_IN_METRIC_OBJECT};
pcep_error(o_bit_cleared_in_rp_object) ->
    {?PCEP_ERRT_POLICY_VIOLATION, ?PCEP_ERRV_O_BIT_CLEARD_IN_RP_OBJECT};
pcep_error(objfun_not_allowed) ->
    {?PCEP_ERRT_POLICY_VIOLATION, ?PCEP_ERRV_OBJFUN_NOT_ALLOWED};
pcep_error(rp_of_bit_set) ->
    {?PCEP_ERRT_POLICY_VIOLATION, ?PCEP_ERRV_RP_OF_BIT_SET};
pcep_error(policy_violation) ->
    {?PCEP_ERRT_POLICY_VIOLATION, ?PCEP_ERRV_UNASSIGNED};
pcep_error(rp_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_RP_OBJECT_MISSING};
pcep_error(rro_object_missing_for_reop) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_RRO_OBJECT_MISSING_FOR_REOP};
pcep_error(ep_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_EP_OBJECT_MISSING};
pcep_error(lsp_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_LSP_OBJECT_MISSING};
pcep_error(ero_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_ERO_OBJECT_MISSING};
pcep_error(srp_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_SRP_OBJECT_MISSING};
pcep_error(lsp_id_tlv_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_LSP_ID_TLV_MISSING};
pcep_error(lsp_db_tlv_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_LSP_DB_TLV_MISSING};
pcep_error(mandatory_object_missing) ->
    {?PCEP_ERRT_MANDATORY_OBJECT_MISSING, ?PCEP_ERRV_UNASSIGNED};
pcep_error(sync_pc_req_missing) ->
    {?PCEP_ERRT_SYNC_PC_REQ_MISSING, ?PCEP_ERRV_UNASSIGNED};
pcep_error(unknown_req_ref) ->
    {?PCEP_ERRT_UNKNOWN_REQ_REF, ?PCEP_ERRV_UNASSIGNED};
pcep_error(second_pcep_session_attempted) ->
    {?PCEP_ERRT_SECOND_PCEP_SESSION_ATTEMPTED, ?PCEP_ERRV_UNASSIGNED};
pcep_error(p_flag_not_correct_in_object) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_P_FLAG_NOT_CORRECT_IN_OBJECT};
pcep_error(bad_label_value) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_BAD_LABEL_VALUE};
pcep_error(unsupported_num_sr_ero_subobjects) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_UNSUPPORTED_NUM_SR_ERO_SUBOBJECTS};
pcep_error(bad_label_format) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_BAD_LABEL_FORMAT};
pcep_error(sr_ero_mixed) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SR_ERO_MIXED};
pcep_error(sr_ero_sid_nai_absent) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SR_ERO_SID_NAI_ABSENT};
pcep_error(sr_rro_sid_nai_absent) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SR_RRO_SID_NAI_ABSENT};
pcep_error(symbolic_path_name_missing) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SYMBOLIC_PATH_NAME_MISSING};
pcep_error(msd_exceeds_default) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_MSD_EXCEEDS_DEFAULT};
pcep_error(sr_rro_mixed) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SR_RRO_MIXED};
pcep_error(malformed_object) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_MALFORMED_OBJECT};
pcep_error(sr_cap_tlv_missing) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SR_CAP_TLV_MISSING};
pcep_error(unsupported_nai_type) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_UNSUPPORTED_NAI_TYPE};
pcep_error(unknown_sid) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_UNKNOWN_SID};
pcep_error(cannot_resolve_nai) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_CANNOT_RESOLVE_NAI};
pcep_error(srgb_not_found) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SRGB_NOT_FOUND};
pcep_error(sid_exceeds_srgb_size) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SID_EXCEEDS_SRGB_SIZE};
pcep_error(srlb_not_found) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SRLB_NOT_FOUND};
pcep_error(sid_exceeds_srlb_size) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_SID_EXCEEDS_SRLB_SIZE};
pcep_error(inconsistent_sids) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_INCONSISTENT_SIDS};
pcep_error(msd_must_be_nonzero) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_MSD_MUST_BE_NONZERO};
pcep_error(invalid_object) ->
    {?PCEP_ERRT_INVALID_OBJECT, ?PCEP_ERRV_UNASSIGNED};
pcep_error(lsp_update_for_non_delegated_lsp) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_LSP_UPDATE_FOR_NON_DELEGATED_LSP};
pcep_error(lsp_update_non_advertised_pce) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_LSP_UPDATE_NON_ADVERTISED_PCE};
pcep_error(lsp_update_unknown_plsp_id) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_LSP_UPDATE_UNKNOWN_PLSP_ID};
pcep_error(lsp_report_non_advertised_pce) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_LSP_REPORT_NON_ADVERTISED_PCE};
pcep_error(pce_init_lsp_limit_reached) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_PCE_INIT_LSP_LIMIT_REACHED};
pcep_error(pce_init_lsp_delegation_cant_revoke) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_PCE_INIT_LSP_DELEGATION_CANT_REVOKE};
pcep_error(lsp_init_non_zero_plsp_id) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_LSP_INIT_NON_ZERO_PLSP_ID};
pcep_error(lsp_not_pce_initiated) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_LSP_NOT_PCE_INITIATED};
pcep_error(pce_init_op_freq_limit_reached) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_PCE_INIT_OP_FREQ_LIMIT_REACHED};
pcep_error(invalid_operation) ->
    {?PCEP_ERRT_INVALID_OPERATION, ?PCEP_ERRV_UNASSIGNED};
pcep_error(pce_cant_process_lsp_report) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_PCE_CANT_PROCESS_LSP_REPORT};
pcep_error(lsp_db_version_mismatch) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_LSP_DB_VERSION_MISMATCH};
pcep_error(trigger_attempt_before_pce_trigger) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_TRIGGER_ATTEMPT_BEFORE_PCE_TRIGGER};
pcep_error(trigger_attempt_no_pce_trigger_cap) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_TRIGGER_ATTEMPT_NO_PCE_TRIGGER_CAP};
pcep_error(pcc_cant_complete_state_sync) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_PCC_CANT_COMPLETE_STATE_SYNC};
pcep_error(invalid_lsp_db_version_number) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_INVALID_LSP_DB_VERSION_NUMBER};
pcep_error(invalid_speaker_entity_id) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_INVALID_SPEAKER_ENTITY_ID};
pcep_error(lsp_state_sync_error) ->
    {?PCEP_ERRT_LSP_STATE_SYNC_ERROR, ?PCEP_ERRV_UNASSIGNED};
pcep_error(unsupported_path_setup_type) ->
    {?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE, ?PCEP_ERRV_UNSUPPORTED_PATH_SETUP_TYPE};
pcep_error(mismatched_path_setup_type) ->
    {?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE, ?PCEP_ERRV_MISMATCHED_PATH_SETUP_TYPE};
pcep_error(invalid_te_path_setup_type) ->
    {?PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE, ?PCEP_ERRV_UNASSIGNED};
pcep_error(symbolic_path_name_in_use) ->
    {?PCEP_ERRT_BAD_PARAMETER_VALUE, ?PCEP_ERRV_SYMBOLIC_PATH_NAME_IN_USE};
pcep_error(lsp_speaker_id_not_pce_initiated) ->
    {?PCEP_ERRT_BAD_PARAMETER_VALUE, ?PCEP_ERRV_LSP_SPEAKER_ID_NOT_PCE_INITIATED};
pcep_error(bad_parameter_value) ->
    {?PCEP_ERRT_BAD_PARAMETER_VALUE, ?PCEP_ERRV_UNASSIGNED};
pcep_error(unacceptable_instantiate_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR, ?PCEP_ERRV_UNACCEPTABLE_INSTANTIATE_ERROR};
pcep_error(internal_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR, ?PCEP_ERRV_INTERNAL_ERROR};
pcep_error(signalling_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR, ?PCEP_ERRV_SIGNALLING_ERROR};
pcep_error(lsp_instantiate_error) ->
    {?PCEP_ERRT_LSP_INSTANTIATE_ERROR, ?PCEP_ERRV_UNASSIGNED};
pcep_error(assoc_type_not_supported) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_ASSOC_TYPE_NOT_SUPPORTED};
pcep_error(too_many_lsps_in_assoc_grp) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_TOO_MANY_LSPS_IN_ASSOC_GRP};
pcep_error(too_many_assoc_groups) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_TOO_MANY_ASSOC_GROUPS};
pcep_error(association_unknown) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_ASSOCIATION_UNKNOWN};
pcep_error(op_conf_assoc_info_mismatch) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_OP_CONF_ASSOC_INFO_MISMATCH};
pcep_error(assoc_info_mismatch) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_ASSOC_INFO_MISMATCH};
pcep_error(cannot_join_assoc_group) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_CANNOT_JOIN_ASSOC_GROUP};
pcep_error(assoc_id_not_in_range) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_ASSOC_ID_NOT_IN_RANGE};
pcep_error(association_error) ->
    {?PCEP_ERRT_ASSOCIATION_ERROR, ?PCEP_ERRV_UNASSIGNED}.

-spec int_close_reason(non_neg_integer())
    -> pcep_close_reason() | undefined.
int_close_reason(?PCEP_CLOSE_REASON_NO_REASON) -> no_reason;
int_close_reason(?PCEP_CLOSE_REASON_DEADTIMER) -> deadtimer;
int_close_reason(?PCEP_CLOSE_REASON_FORMAT) -> format;
int_close_reason(?PCEP_CLOSE_REASON_TOO_MUCH_BAD_REQ) -> too_much_bad_req;
int_close_reason(?PCEP_CLOSE_REASON_TOO_MUCH_BAD_MSG) -> too_much_bad_msg;
int_close_reason(_Other) -> undefined.

-spec pcep_close_reason(pcep_close_reason()) -> non_neg_integer().
pcep_close_reason(no_reason) -> ?PCEP_CLOSE_REASON_NO_REASON;
pcep_close_reason(deadtimer) -> ?PCEP_CLOSE_REASON_DEADTIMER;
pcep_close_reason(format) -> ?PCEP_CLOSE_REASON_FORMAT;
pcep_close_reason(too_much_bad_req) -> ?PCEP_CLOSE_REASON_TOO_MUCH_BAD_REQ;
pcep_close_reason(too_much_bad_msg) -> ?PCEP_CLOSE_REASON_TOO_MUCH_BAD_MSG.

-spec int_lsp_error(non_neg_integer()) -> pcep_lsp_error() | undefined.
int_lsp_error(?PCEP_LSP_ERROR_UNKNOWN) ->
    unknown;
int_lsp_error(?PCEP_LSP_ERROR_LIMIT_REACHED) ->
    limit_reached;
int_lsp_error(?PCEP_LSP_ERROR_TOO_MANY_PENDING_UPDATES) ->
    too_many_pending_updates;
int_lsp_error(?PCEP_LSP_ERROR_UNACCEPTABLE_PARAMETERS) ->
    unacceptable_parameters;
int_lsp_error(?PCEP_LSP_ERROR_INTERNAL_ERROR) ->
    internal_error;
int_lsp_error(?PCEP_LSP_ERROR_ADMIN_BROUGHT_DOWN) ->
    admin_brought_down;
int_lsp_error(?PCEP_LSP_ERROR_PREEMPTED) ->
    preempted;
int_lsp_error(?PCEP_LSP_ERROR_RSVP_SIGNALING_ERROR) ->
    rsvp_signaling_error;
int_lsp_error(_Other) ->
    undefined.

-spec pcep_lsp_error(pcep_lsp_error()) -> non_neg_integer().
pcep_lsp_error(unknown) ->
    ?PCEP_LSP_ERROR_UNKNOWN;
pcep_lsp_error(limit_reached) ->
    ?PCEP_LSP_ERROR_LIMIT_REACHED;
pcep_lsp_error(too_many_pending_updates) ->
    ?PCEP_LSP_ERROR_TOO_MANY_PENDING_UPDATES;
pcep_lsp_error(unacceptable_parameters) ->
    ?PCEP_LSP_ERROR_UNACCEPTABLE_PARAMETERS;
pcep_lsp_error(internal_error) ->
    ?PCEP_LSP_ERROR_INTERNAL_ERROR;
pcep_lsp_error(admin_brought_down) ->
    ?PCEP_LSP_ERROR_ADMIN_BROUGHT_DOWN;
pcep_lsp_error(preempted) ->
    ?PCEP_LSP_ERROR_PREEMPTED;
pcep_lsp_error(rsvp_signaling_error) ->
    ?PCEP_LSP_ERROR_RSVP_SIGNALING_ERROR.

-spec int_pst(non_neg_integer()) -> pcep_pst() | undefined.
int_pst(?PCEP_PST_RSVPTE) -> rsvpte;
int_pst(?PCEP_PST_SRTE) -> srte;
int_pst(_Other) -> undefined.

-spec pcep_pst(pcep_pst()) -> non_neg_integer().
pcep_pst(rsvpte) -> ?PCEP_PST_RSVPTE;
pcep_pst(srte) -> ?PCEP_PST_SRTE.

-spec int_nai_type(non_neg_integer()) -> srte_nai_type() | undefined.
int_nai_type(?PCEP_SR_SUBOBJ_NAI_ABSENT) ->
    absent;
int_nai_type(?PCEP_SR_SUBOBJ_NAI_IPV4_NODE) ->
    ipv4_node;
int_nai_type(?PCEP_SR_SUBOBJ_NAI_IPV6_NODE) ->
    ipv6_node;
int_nai_type(?PCEP_SR_SUBOBJ_NAI_IPV4_ADJACENCY) ->
    ipv4_adjacency;
int_nai_type(?PCEP_SR_SUBOBJ_NAI_IPV6_ADJACENCY) ->
    ipv6_adjacency;
int_nai_type(?PCEP_SR_SUBOBJ_NAI_UNNUMBERED_IPV4_ADJACENCY) ->
    unumbered_ipv4_adjacency;
int_nai_type(?PCEP_SR_SUBOBJ_NAI_LINKLOCAL_IPV6_ADJACENCY) ->
    linklocal_ipv6_adjacency;
int_nai_type(_Other) ->
    undefined.

-spec pcep_nai_type(srte_nai_type()) -> non_neg_integer().
pcep_nai_type(absent) ->
    ?PCEP_SR_SUBOBJ_NAI_ABSENT;
pcep_nai_type(ipv4_node) ->
    ?PCEP_SR_SUBOBJ_NAI_IPV4_NODE;
pcep_nai_type(ipv6_node) ->
    ?PCEP_SR_SUBOBJ_NAI_IPV6_NODE;
pcep_nai_type(ipv4_adjacency) ->
    ?PCEP_SR_SUBOBJ_NAI_IPV4_ADJACENCY;
pcep_nai_type(ipv6_adjacency) ->
    ?PCEP_SR_SUBOBJ_NAI_IPV6_ADJACENCY;
pcep_nai_type(unumbered_ipv4_adjacency) ->
    ?PCEP_SR_SUBOBJ_NAI_UNNUMBERED_IPV4_ADJACENCY;
pcep_nai_type(linklocal_ipv6_adjacency) ->
    ?PCEP_SR_SUBOBJ_NAI_LINKLOCAL_IPV6_ADJACENCY.

-spec int_opstatus(non_neg_integer()) -> te_opstatus() | undefined.
int_opstatus(?PCEP_OPSTATUS_DOWN) -> down;
int_opstatus(?PCEP_OPSTATUS_UP) -> up;
int_opstatus(?PCEP_OPSTATUS_ACTIVE) -> active;
int_opstatus(?PCEP_OPSTATUS_GOING_DOWN) -> going_down;
int_opstatus(?PCEP_OPSTATUS_GOING_UP) -> going_up;
int_opstatus(_Other) -> undefined.

-spec pcep_opstatus(te_opstatus()) -> non_neg_integer().
pcep_opstatus(down) -> ?PCEP_OPSTATUS_DOWN;
pcep_opstatus(up) -> ?PCEP_OPSTATUS_UP;
pcep_opstatus(active) -> ?PCEP_OPSTATUS_ACTIVE;
pcep_opstatus(going_down) -> ?PCEP_OPSTATUS_GOING_DOWN;
pcep_opstatus(going_up) -> ?PCEP_OPSTATUS_GOING_UP.

-spec error_type_from_value(pcep_error_value()) -> pcep_error_type().
error_type_from_value(invalid_open_message) -> session_failure;
error_type_from_value(openwait_timed_out) -> session_failure;
error_type_from_value(unacceptable_open_msg_no_neg) -> session_failure;
error_type_from_value(unacceptable_open_msg_neg) -> session_failure;
error_type_from_value(second_open_msg_unacceptable) -> session_failure;
error_type_from_value(recvd_pcerr) -> session_failure;
error_type_from_value(keepalivewait_timed_out) -> session_failure;
error_type_from_value(pcep_version_not_supported) -> session_failure;
error_type_from_value(capability_not_supported) -> capability_not_supported;
error_type_from_value(unrec_object_class) -> unknown_object;
error_type_from_value(unrec_object_type) -> unknown_object;
error_type_from_value(not_supported_object_class) -> not_supported_object;
error_type_from_value(not_supported_object_type) -> not_supported_object;
error_type_from_value(c_bit_set_in_metric_object) -> policy_violation;
error_type_from_value(o_bit_cleared_in_rp_object) -> policy_violation;
error_type_from_value(objfun_not_allowed) -> policy_violation;
error_type_from_value(rp_of_bit_set) -> policy_violation;
error_type_from_value(rp_object_missing) -> mandatory_object_missing;
error_type_from_value(rro_object_missing_for_reop) -> mandatory_object_missing;
error_type_from_value(ep_object_missing) -> mandatory_object_missing;
error_type_from_value(lsp_object_missing) -> mandatory_object_missing;
error_type_from_value(ero_object_missing) -> mandatory_object_missing;
error_type_from_value(srp_object_missing) -> mandatory_object_missing;
error_type_from_value(lsp_id_tlv_missing) -> mandatory_object_missing;
error_type_from_value(lsp_db_tlv_missing) -> mandatory_object_missing;
error_type_from_value(unknown_req_ref) -> unknown_req_ref;
error_type_from_value(second_pcep_session_attempted) -> second_pcep_session_attempted;
error_type_from_value(p_flag_not_correct_in_object) -> invalid_object;
error_type_from_value(bad_label_value) -> invalid_object;
error_type_from_value(unsupported_num_sr_ero_subobjects) -> invalid_object;
error_type_from_value(bad_label_format) -> invalid_object;
error_type_from_value(sr_ero_mixed) -> invalid_object;
error_type_from_value(sr_ero_sid_nai_absent) -> invalid_object;
error_type_from_value(sr_rro_sid_nai_absent) -> invalid_object;
error_type_from_value(symbolic_path_name_missing) -> invalid_object;
error_type_from_value(msd_exceeds_default) -> invalid_object;
error_type_from_value(sr_rro_mixed) -> invalid_object;
error_type_from_value(malformed_object) -> invalid_object;
error_type_from_value(sr_cap_tlv_missing) -> invalid_object;
error_type_from_value(unsupported_nai_type) -> invalid_object;
error_type_from_value(unknown_sid) -> invalid_object;
error_type_from_value(cannot_resolve_nai) -> invalid_object;
error_type_from_value(srgb_not_found) -> invalid_object;
error_type_from_value(sid_exceeds_srgb_size) -> invalid_object;
error_type_from_value(srlb_not_found) -> invalid_object;
error_type_from_value(sid_exceeds_srlb_size) -> invalid_object;
error_type_from_value(inconsistent_sids) -> invalid_object;
error_type_from_value(msd_must_be_nonzero) -> invalid_object;
error_type_from_value(lsp_update_for_non_delegated_lsp) -> invalid_operation;
error_type_from_value(lsp_update_non_advertised_pce) -> invalid_operation;
error_type_from_value(lsp_update_unknown_plsp_id) -> invalid_operation;
error_type_from_value(lsp_report_non_advertised_pce) -> invalid_operation;
error_type_from_value(pce_init_lsp_limit_reached) -> invalid_operation;
error_type_from_value(pce_init_lsp_delegation_cant_revoke) -> invalid_operation;
error_type_from_value(lsp_init_non_zero_plsp_id) -> invalid_operation;
error_type_from_value(lsp_not_pce_initiated) -> invalid_operation;
error_type_from_value(pce_init_op_freq_limit_reached) -> invalid_operation;
error_type_from_value(pce_cant_process_lsp_report) -> lsp_state_sync_error;
error_type_from_value(lsp_db_version_mismatch) -> lsp_state_sync_error;
error_type_from_value(trigger_attempt_before_pce_trigger) -> lsp_state_sync_error;
error_type_from_value(trigger_attempt_no_pce_trigger_cap) -> lsp_state_sync_error;
error_type_from_value(pcc_cant_complete_state_sync) -> lsp_state_sync_error;
error_type_from_value(invalid_lsp_db_version_number) -> lsp_state_sync_error;
error_type_from_value(invalid_speaker_entity_id) -> lsp_state_sync_error;
error_type_from_value(unsupported_path_setup_type) -> invalid_te_path_setup_type;
error_type_from_value(mismatched_path_setup_type) -> invalid_te_path_setup_type;
error_type_from_value(symbolic_path_name_in_use) -> bad_parameter_value;
error_type_from_value(lsp_speaker_id_not_pce_initiated) -> bad_parameter_value;
error_type_from_value(unacceptable_instantiate_error) -> lsp_instantiate_error;
error_type_from_value(internal_error) -> lsp_instantiate_error;
error_type_from_value(signalling_error) -> lsp_instantiate_error;
error_type_from_value(assoc_type_not_supported) -> association_error;
error_type_from_value(too_many_lsps_in_assoc_grp) -> association_error;
error_type_from_value(too_many_assoc_groups) -> association_error;
error_type_from_value(association_unknown) -> association_error;
error_type_from_value(op_conf_assoc_info_mismatch) -> association_error;
error_type_from_value(assoc_info_mismatch) -> association_error;
error_type_from_value(cannot_join_assoc_group) -> association_error;
error_type_from_value(assoc_id_not_in_range) -> association_error.
