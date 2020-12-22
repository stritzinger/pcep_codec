
%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type ip4_prefix() :: {inet:ip4_address(), 0..32}.
-type ip6_prefix() :: {inet:ip6_address(), 0..128}.

-type pcep_nopath_nature() :: path_not_found | pce_chain_broken.
-type pcep_metric_type() :: igp | te | hop_count | aggregate_bw
                          | most_loaded_link | cumulative_igp | cumulative_te
                          | segment_id_depth.

-type pcep_notif_type() :: req_canceled
                         | pce_congestion
                         | pce_limit_exceeded.

-type pcep_notif_value() :: pce_canceled_req
                          | pce_congested
                          | pce_recovered
                          | pce_limit_exceeded.

-type pcep_error_type() :: session_failure
                         | capability_not_supported
                         | unknown_object
                         | not_supported_object
                         | policy_violation
                         | mandatory_object_missing
                         | unknown_req_ref
                         | second_pcep_session_attempted
                         | invalid_object
                         | invalid_operation
                         | lsp_state_sync_error
                         | invalid_te_path_setup_type
                         | bad_parameter_value
                         | lsp_instantiate_error
                         | association_error.

-type pcep_error_value() :: invalid_open_message
                          | openwait_timed_out
                          | unacceptable_open_msg_no_neg
                          | unacceptable_open_msg_neg
                          | second_open_msg_unacceptable
                          | recvd_pcerr
                          | keepalivewait_timed_out
                          | pcep_version_not_supported
                          | session_failure
                          | capability_not_supported
                          | unrec_object_class
                          | unrec_object_type
                          | unknown_object
                          | not_supported_object_class
                          | not_supported_object_type
                          | not_supported_object
                          | c_bit_set_in_metric_object
                          | o_bit_cleared_in_rp_object
                          | objfun_not_allowed
                          | rp_of_bit_set
                          | policy_violation
                          | rp_object_missing
                          | rro_object_missing_for_reop
                          | ep_object_missing
                          | lsp_object_missing
                          | ero_object_missing
                          | srp_object_missing
                          | lsp_id_tlv_missing
                          | lsp_db_tlv_missing
                          | mandatory_object_missing
                          | sync_pc_req_missing
                          | unknown_req_ref
                          | second_pcep_session_attempted
                          | p_flag_not_correct_in_object
                          | bad_label_value
                          | unsupported_num_sr_ero_subobjects
                          | bad_label_format
                          | sr_ero_mixed
                          | sr_ero_sid_nai_absent
                          | sr_rro_sid_nai_absent
                          | symbolic_path_name_missing
                          | msd_exceeds_default
                          | sr_rro_mixed
                          | malformed_object
                          | sr_cap_tlv_missing
                          | unsupported_nai_type
                          | unknown_sid
                          | cannot_resolve_nai
                          | srgb_not_found
                          | sid_exceeds_srgb_size
                          | srlb_not_found
                          | sid_exceeds_srlb_size
                          | inconsistent_sids
                          | msd_must_be_nonzero
                          | invalid_object
                          | lsp_update_for_non_delegated_lsp
                          | lsp_update_non_advertised_pce
                          | lsp_update_unknown_plsp_id
                          | lsp_report_non_advertised_pce
                          | pce_init_lsp_limit_reached
                          | pce_init_lsp_delegation_cant_revoke
                          | lsp_init_non_zero_plsp_id
                          | lsp_not_pce_initiated
                          | pce_init_op_freq_limit_reached
                          | invalid_operation
                          | pce_cant_process_lsp_report
                          | lsp_db_version_mismatch
                          | trigger_attempt_before_pce_trigger
                          | trigger_attempt_no_pce_trigger_cap
                          | pcc_cant_complete_state_sync
                          | invalid_lsp_db_version_number
                          | invalid_speaker_entity_id
                          | lsp_state_sync_error
                          | unsupported_path_setup_type
                          | mismatched_path_setup_type
                          | invalid_te_path_setup_type
                          | symbolic_path_name_in_use
                          | lsp_speaker_id_not_pce_initiated
                          | bad_parameter_value
                          | unacceptable_instantiate_error
                          | internal_error
                          | signalling_error
                          | lsp_instantiate_error
                          | assoc_type_not_supported
                          | too_many_lsps_in_assoc_grp
                          | too_many_assoc_groups
                          | association_unknown
                          | op_conf_assoc_info_mismatch
                          | assoc_info_mismatch
                          | cannot_join_assoc_group
                          | assoc_id_not_in_range
                          | association_error.

-type pcep_close_reason() :: no_reason
                           | deadtimer
                           | format
                           | too_much_bad_req
                           | too_much_bad_msg.

-type pcep_lsp_error() :: unknown
                        | limit_reached
                        | too_many_pending_updates
                        | unacceptable_parameters
                        | internal_error
                        | admin_brought_down
                        | preempted
                        | rsvp_signaling_error.

-type pcep_pst() :: rsvpte | srte.

-type pcep_nai_type() :: absent
                       | ipv4_node
                       | ipv6_node
                       | ipv4_adjacency
                       | ipv6_adjacency
                       | unumbered_ipv4_adjacency
                       | linklocal_ipv6_adjacency.

-type pcep_sid() :: non_neg_integer().

-type pcep_opstatus() :: down
                       | up
                       | active
                       | going_down
                       | going_up.


%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(mpls_stack_entry, {
    label = 0 :: 0..1048575,
    tc  = 0 :: 0..7,
    is_last = false :: boolean(),
    ttl = 0 :: 0..255
}).

-type mpls_stack_entry() :: #mpls_stack_entry{}.

-record(sr_nai_node, {
    address :: inet:ip_address()
}).

-record(sr_nai_adjacency, {
    local_address :: inet:ip_address(),
    remote_address :: inet:ip_address()
}).

-record(sr_nai_unumbered_ipv4_adjacency, {
    local_node_id :: non_neg_integer(),
    local_interface_id :: non_neg_integer(),
    remote_node_id :: non_neg_integer(),
    remote_interface_id :: non_neg_integer()
}).

-record(sr_nai_linklocal_ipv6_adjacency, {
    local_address :: inet:ip6_address(),
    local_interface_id :: non_neg_integer(),
    remote_address :: inet:ip6_address(),
    remote_interface_id :: non_neg_integer()
}).

-type pcep_nai() :: #sr_nai_node{}
                  | #sr_nai_adjacency{}
                  | #sr_nai_unumbered_ipv4_adjacency{}
                  | #sr_nai_linklocal_ipv6_adjacency{}.


%-- PCEP SUB-TLVS --------------------------------------------------------------

-record(pcep_subtlv_unknown, {
    type = 0 :: non_neg_integer(),
    data = <<>> :: binary()
}).

-record(pcep_subtlv_path_setup_type_cap_sr, {
    flag_n = false :: boolean(),
    flag_x = false :: boolean(),
    msd = 0 :: 0..255
}).

-type pcep_subtlvs() :: #pcep_subtlv_unknown{}
                      | #pcep_subtlv_path_setup_type_cap_sr{}.


%-- PCEP TLVS ------------------------------------------------------------------

-record(pcep_tlv_unknown, {
    type = 0 :: non_neg_integer(),
    data = <<>> :: binary()
}).

-record(pcep_tlv_nopath_vector, {
    pce_not_available = false :: boolean(),
    unknown_destination = false :: boolean(),
    unknown_source = false :: boolean()
}).
-record(pcep_tlv_overload_duration, {
    estimated = 0 :: non_neg_integer()
}).
-record(pcep_tlv_req_missing, {
    req_id = 0 :: non_neg_integer()
}).
-record(pcep_tlv_objfun_list, {

}).
-record(pcep_tlv_vendor_info, {

}).
-record(pcep_tlv_stateful_pce_cap, {
    flag_u = false :: boolean(),
    flag_i = false :: boolean()
}).
-record(pcep_tlv_symbolic_path_name, {
    name = <<>> :: binary()
}).
-record(pcep_tlv_ipv4_lsp_id, {
    source = {0, 0, 0, 0} :: inet:ip4_address(),
    lsp_id = 0 :: non_neg_integer(),
    tunnel_id = 0 :: non_neg_integer(),
    extended_id = <<0, 0, 0, 0>> :: binary(),
    endpoint = {0, 0, 0, 0} :: inet:ip4_address()
}).
-record(pcep_tlv_ipv6_lsp_id, {
    source = {0, 0, 0, 0, 0, 0, 0, 0} :: inet:ip6_address(),
    lsp_id = 0 :: non_neg_integer(),
    tunnel_id = 0 :: non_neg_integer(),
    extended_id = <<0:128>> :: binary(),
    endpoint = {0, 0, 0, 0, 0, 0, 0, 0} :: inet:ip6_address()
}).
-record(pcep_tlv_lsp_error_code, {
    error = unknown :: pcep_lsp_error()
}).
-record(pcep_tlv_rsvp_error_spec, {

}).
-record(pcep_tlv_lsp_db_version, {

}).
-record(pcep_tlv_speaker_entity_id, {

}).
-record(pcep_tlv_sr_pce_cap, {
  msd = 0 :: 0..255
}).
-record(pcep_tlv_path_setup_type, {
    pst = rsvpte :: pcep_pst()
}).
-record(pcep_tlv_assoc_range, {

}).
-record(pcep_tlv_global_assoc_source, {

}).
-record(pcep_tlv_ext_assoc_id, {

}).
-record(pcep_tlv_path_setup_type_cap, {
    psts = [] :: [pcep_pst()],
    subtlvs = [] :: [pcep_subtlvs()]
}).
-record(pcep_tlv_assoc_type_list, {

}).

-type pcep_tlv() :: #pcep_tlv_unknown{}
                  | #pcep_tlv_nopath_vector{}
                  | #pcep_tlv_overload_duration{}
                  | #pcep_tlv_req_missing{}
                  | #pcep_tlv_objfun_list{}
                  | #pcep_tlv_vendor_info{}
                  | #pcep_tlv_stateful_pce_cap{}
                  | #pcep_tlv_symbolic_path_name{}
                  | #pcep_tlv_ipv4_lsp_id{}
                  | #pcep_tlv_ipv6_lsp_id{}
                  | #pcep_tlv_lsp_error_code{}
                  | #pcep_tlv_rsvp_error_spec{}
                  | #pcep_tlv_lsp_db_version{}
                  | #pcep_tlv_speaker_entity_id{}
                  | #pcep_tlv_sr_pce_cap{}
                  | #pcep_tlv_path_setup_type{}
                  | #pcep_tlv_assoc_range{}
                  | #pcep_tlv_global_assoc_source{}
                  | #pcep_tlv_ext_assoc_id{}
                  | #pcep_tlv_path_setup_type_cap{}
                  | #pcep_tlv_assoc_type_list{}.

%-- RSVP/PCEP RO SUB-OBJECTS ---------------------------------------------------

-record(pcep_ro_subobj_ipv4, {
    flag_l = false :: boolean(),
    prefix = {{0, 0, 0, 0}, 0} :: ip4_prefix()
}).
-record(pcep_ro_subobj_ipv6, {
    flag_l = false :: boolean(),
    prefix = {{0, 0, 0, 0, 0, 0, 0, 0}, 0} :: ip6_prefix()
}).
-record(pcep_ro_subobj_unum, {
    flag_l = false :: boolean(),
    router_id :: inet:ip4_address(),
    interface_id :: non_neg_integer()
}).
-record(pcep_ro_subobj_asn, {
    flag_l :: boolean(),
    asn :: 0..65535
}).
-record(pcep_ro_subobj_sr, {
    flag_l = false :: boolean(),
    has_sid = false :: boolean(),
    has_nai = false :: boolean(),
    nai_type = absent :: pcep_nai_type(),
    is_mpls = false :: boolean(),
    % If is_mpls is true, this tells if the ttl,
    % is_last and tc are defined by the PCE
    has_mpls_extra = false :: boolean(),
    sid :: pcep_sid() | mpls_stack_entry() | undefined,
    nai :: pcep_nai() | undefined
}).

-type pcep_ro_subobj() :: #pcep_ro_subobj_ipv4{}
                        | #pcep_ro_subobj_ipv6{}
                        | #pcep_ro_subobj_unum{}
                        | #pcep_ro_subobj_asn{}
                        | #pcep_ro_subobj_sr{}.


%-- PCEP OBJECTS ---------------------------------------------------------------

-record(pcep_obj_open, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    keepalive = 0 :: 0..255,
    dead_timer = 0 :: 0..255,
    sid = 0 :: 0..255,
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_open() :: #pcep_obj_open{}.

-record(pcep_obj_rp, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    flag_o = false :: boolean(),
    flag_b = false :: boolean(),
    flag_r = false :: boolean(),
    flag_s = false :: boolean(),
    pri = 0 :: 0..7,
    req_id = 0 :: non_neg_integer(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_rp() :: #pcep_obj_rp{}.

-record(pcep_obj_nopath, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    flag_c = false :: boolean(),
    ni = path_not_found :: pcep_nopath_nature(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_nopath() :: #pcep_obj_nopath{}.

-record(pcep_obj_endpoint_ipv4_addr, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    source = {0, 0, 0, 0} :: inet:ip4_address(),
    destination = {0, 0, 0, 0} :: inet:ip4_address()
}).
-type pcep_obj_endpoint_ipv4_addr() :: #pcep_obj_endpoint_ipv4_addr{}.

-record(pcep_obj_endpoint_ipv6_addr, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    source = {0, 0, 0, 0, 0, 0, 0, 0} :: inet:ip6_address(),
    destination = {0, 0, 0, 0, 0, 0, 0, 0} :: inet:ip6_address()
}).
-type pcep_obj_endpoint_ipv6_addr() :: #pcep_obj_endpoint_ipv6_addr{}.

% All metric records MUST be identical, the object decoding code expects it
-record(pcep_obj_bandwidth_req, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    bandwidth = 0.0 :: float()
}).
-type pcep_obj_bandwidth_req() :: #pcep_obj_bandwidth_req{}.

-record(pcep_obj_bandwidth_telsp, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    bandwidth = 0.0 :: float()
}).
-type pcep_obj_bandwidth_telsp() :: #pcep_obj_bandwidth_telsp{}.

-record(pcep_obj_bandwidth_cisco, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    bandwidth = 0.0 :: float()
}).
-type pcep_obj_bandwidth_cisco() :: #pcep_obj_bandwidth_cisco{}.

-record(pcep_obj_metric, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    flag_c = false :: boolean(),
    flag_b = false :: boolean(),
    type = igp :: pcep_metric_type(),
    value = 0.0 ::float()
}).
-type pcep_obj_metric() :: #pcep_obj_metric{}.

% All RO records MUST be identical, the object decoding code expects it
-record(pcep_obj_ero, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    path = [] :: [pcep_ro_subobj()]
}).
-type pcep_obj_ero() :: #pcep_obj_ero{}.

-record(pcep_obj_rro, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    path = [] :: [pcep_ro_subobj()]
}).
-type pcep_obj_rro() :: #pcep_obj_rro{}.

-record(pcep_obj_iro, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    path = [] :: [pcep_ro_subobj()]
}).
-type pcep_obj_iro() :: #pcep_obj_iro{}.

-record(pcep_obj_lspa, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    flag_l = false :: boolean(),
    exclude_any = 0 :: non_neg_integer(),
    include_any = 0 :: non_neg_integer(),
    include_all = 0 :: non_neg_integer(),
    setup_prio = 0 :: 0..255,
    holding_prio = 0 :: 0..255,
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_lspa() :: #pcep_obj_lspa{}.

%TODO: Implement SVEC
-record(pcep_obj_svec, {
}).
-type pcep_obj_svec() :: #pcep_obj_svec{}.

-record(pcep_obj_notif, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    type = req_canceled :: pcep_notif_type(),
    value = pce_canceled_req:: pcep_notif_value(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_notif() :: #pcep_obj_notif{}.

-record(pcep_obj_error, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    type = session_failure :: pcep_error_type(),
    value = invalid_open_message :: pcep_error_value(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_error() :: #pcep_obj_error{}.

-record(pcep_obj_load_balancing, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    max_lsp = 0 :: 0..255,
    min_bandwidth = 0.0 :: float()
}).
-type pcep_obj_load_balancing() :: #pcep_obj_load_balancing{}.

-record(pcep_obj_close, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    reason = no_reason :: pcep_close_reason(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_close() :: #pcep_obj_close{}.

-record(pcep_obj_objfun, {

}).
-type pcep_obj_objfun() :: #pcep_obj_objfun{}.

-record(pcep_obj_lsp, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    flag_d = false :: boolean(),
    flag_s = false :: boolean(),
    flag_r = false :: boolean(),
    flag_a = false :: boolean(),
    flag_c = false :: boolean(),
    plsp_id = 0 :: non_neg_integer(),
    status = down :: pcep_opstatus(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_lsp() :: #pcep_obj_lsp{}.

-record(pcep_obj_srp, {
    flag_p = false :: boolean(),
    flag_i = false :: boolean(),
    srp_id = 0 :: non_neg_integer(),
    tlvs = [] :: [pcep_tlv()]
}).
-type pcep_obj_srp() :: #pcep_obj_srp{}.

-record(pcep_obj_vendor_info, {

}).
-type pcep_obj_vendor_info() :: #pcep_obj_vendor_info{}.

-record(pcep_obj_inter_layer, {

}).
-type pcep_obj_inter_layer() :: #pcep_obj_inter_layer{}.

-record(pcep_obj_switch_layer, {

}).
-type pcep_obj_switch_layer() :: #pcep_obj_switch_layer{}.

-record(pcep_obj_req_adap_cap, {

}).
-type pcep_obj_req_adap_cap() :: #pcep_obj_req_adap_cap{}.

-record(pcep_obj_server_indication, {

}).
-type pcep_obj_server_indication() :: #pcep_obj_server_indication{}.

-record(pcep_obj_association_ipv4, {

}).
-type pcep_obj_association_ipv4() :: #pcep_obj_association_ipv4{}.

-record(pcep_obj_association_ipv6, {

}).
-type pcep_obj_association_ipv6() :: #pcep_obj_association_ipv6{}.

-record(pcep_obj_undefined, {

}).
-type pcep_obj_undefined() :: #pcep_obj_undefined{}.


-type pcep_object() :: #pcep_obj_open{}
                     | #pcep_obj_rp{}
                     | #pcep_obj_nopath{}
                     | #pcep_obj_endpoint_ipv4_addr{}
                     | #pcep_obj_endpoint_ipv6_addr{}
                     | #pcep_obj_bandwidth_req{}
                     | #pcep_obj_bandwidth_telsp{}
                     | #pcep_obj_bandwidth_cisco{}
                     | #pcep_obj_metric{}
                     | #pcep_obj_ero{}
                     | #pcep_obj_rro{}
                     | #pcep_obj_lspa{}
                     | #pcep_obj_iro{}
                     | #pcep_obj_svec{}
                     | #pcep_obj_notif{}
                     | #pcep_obj_error{}
                     | #pcep_obj_load_balancing{}
                     | #pcep_obj_close{}
                     | #pcep_obj_objfun{}
                     | #pcep_obj_lsp{}
                     | #pcep_obj_srp{}
                     | #pcep_obj_vendor_info{}
                     | #pcep_obj_inter_layer{}
                     | #pcep_obj_switch_layer{}
                     | #pcep_obj_req_adap_cap{}
                     | #pcep_obj_server_indication{}
                     | #pcep_obj_association_ipv4{}
                     | #pcep_obj_association_ipv6{}
                     | #pcep_obj_undefined{}.

-type pcep_obj_endpoint() :: #pcep_obj_endpoint_ipv4_addr{}
                           | #pcep_obj_endpoint_ipv6_addr{}.

-type pcep_obj_bandwidth() :: #pcep_obj_bandwidth_req{}
                            | #pcep_obj_bandwidth_telsp{}
                            | #pcep_obj_bandwidth_cisco{}.

-type pcep_obj_association() :: #pcep_obj_association_ipv4{}
                              | #pcep_obj_association_ipv6{}.


%-- PCEP AGGREGATE RECORDS -----------------------------------------------------

-record(pcep_rpath, {
    rro :: pcep_obj_rro() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    bandwidth :: pcep_obj_bandwidth() | undefined
}).
-type pcep_rpath() :: #pcep_rpath{}.

-record(pcep_path, {
    ero :: pcep_obj_ero() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    lspa :: pcep_obj_lspa() | undefined,
    metrics = [] :: [pcep_obj_metric()],
    iro :: pcep_obj_iro() | undefined,
    bandwidth :: pcep_obj_bandwidth() | undefined
}).
-type pcep_path() :: #pcep_path{}.

-record(pcep_endpoint, {
    endpoint :: pcep_obj_endpoint() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    paths = [] :: [pcep_path()] | [pcep_rpath()],
    bandwidth :: pcep_obj_bandwidth() | undefined
}).
-type pcep_endpoint() :: #pcep_endpoint{}.

-record(pcep_compreq, {
    rp :: pcep_obj_rp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    endpoints = [] :: [pcep_endpoint()],
    lsp :: pcep_obj_lsp() | undefined,
    lspa :: pcep_obj_lspa() | undefined,
    bandwidth :: pcep_obj_bandwidth() | undefined,
    metrics = [] :: [pcep_obj_metric()],
    rpath :: pcep_rpath() | undefined,
    iro :: pcep_obj_iro() | undefined,
    load_balancing :: pcep_obj_load_balancing() | undefined
}).
-type pcep_compreq() :: #pcep_compreq{}.

-record(pcep_comprep, {
    rp :: pcep_obj_rp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    endpoints = [] :: [pcep_endpoint()],
    lsp :: pcep_obj_lsp() | undefined,
    nopath :: pcep_obj_nopath() | undefined,
    lspa :: pcep_obj_lspa() | undefined,
    metrics = [] :: [pcep_obj_metric()],
    bandwidth :: pcep_obj_bandwidth() | undefined,
    iro :: pcep_obj_iro() | undefined
}).
-type pcep_comprep() :: #pcep_comprep{}.

-record(pcep_notif, {
    id_list = [] :: [pcep_obj_rp()],
    notifications = [] :: [pcep_obj_notif()]
}).
-type pcep_notif() :: #pcep_notif{}.

-record(pcep_error, {
    id_list = [] :: [pcep_obj_rp()] | [pcep_obj_srp()],
    errors = [] :: [pcep_obj_error()],
    open :: pcep_obj_open() | undefined
}).
-type pcep_error() :: #pcep_error{}.

-record(pcep_report, {
    srp :: pcep_obj_srp() | undefined,
    lsp :: pcep_obj_lsp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    ero :: pcep_obj_ero() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    rro :: pcep_obj_rro() | undefined,
    actual_bandwidth :: pcep_obj_bandwidth() | undefined,
    actual_metrics = [] :: [pcep_obj_metric()],
    lspa :: pcep_obj_lspa() | undefined,
    bandwidth :: pcep_obj_bandwidth() | undefined,
    metrics = [] :: [pcep_obj_metric()],
    iro :: pcep_obj_iro() | undefined
}).
-type pcep_report() :: #pcep_report{}.

-record(pcep_update, {
    srp :: pcep_obj_srp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    lsp :: pcep_obj_lsp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    ero :: pcep_obj_ero() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    bandwidth :: pcep_obj_bandwidth() | undefined,
    metrics = [] :: [pcep_obj_metric()]
}).
-type pcep_update() :: #pcep_update{}.

-record(pcep_lsp_delete, {
    srp :: pcep_obj_srp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    lsp :: pcep_obj_lsp() | undefined % ONLY UNDEFINED FOR INITIALIZATION
}).
-type pcep_lsp_delete() :: #pcep_lsp_delete{}.

-record(pcep_lsp_init, {
    srp :: pcep_obj_srp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    lsp :: pcep_obj_lsp() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    endpoint :: pcep_obj_endpoint() | undefined,
    ero :: pcep_obj_ero() | undefined, % ONLY UNDEFINED FOR INITIALIZATION
    lspa :: pcep_obj_lspa() | undefined,
    bandwidth :: pcep_obj_bandwidth() | undefined,
    metrics = [] :: [pcep_obj_metric()],
    iro :: pcep_obj_iro() | undefined

}).
-type pcep_lsp_init() :: #pcep_lsp_init{}.


%-- PCEP MESSAGES --------------------------------------------------------------

-record(pcep_msg_open, {
    open :: pcep_obj_open() | undefined % ONLY UNDEFINED FOR INITIALIZATION
}).
-record(pcep_msg_keepalive, {}).
-record(pcep_msg_compreq, {
    request_list = [] :: [pcep_compreq()]
}).
-record(pcep_msg_comprep, {
    reply_list = [] :: [pcep_comprep()]
}).
-record(pcep_msg_notif, {
    notif_list = [] :: [pcep_notif()]
}).
-record(pcep_msg_error, {
    error_list = [] :: [pcep_error()]
}).
-record(pcep_msg_close, {
    close :: pcep_obj_close() | undefined % ONLY UNDEFINED FOR INITIALIZATION
}).
-record(pcep_msg_report, {
    report_list = [] :: [pcep_report()]
}).
-record(pcep_msg_update, {
    update_list = [] :: [pcep_update()]
}).
-record(pcep_msg_initiate, {
    action_list = [] :: [pcep_lsp_init() | pcep_lsp_delete()]
}).

-type pcep_message() :: #pcep_msg_open{}
                      | #pcep_msg_keepalive{}
                      | #pcep_msg_compreq{}
                      | #pcep_msg_comprep{}
                      | #pcep_msg_notif{}
                      | #pcep_msg_error{}
                      | #pcep_msg_close{}
                      | #pcep_msg_report{}
                      | #pcep_msg_update{}
                      | #pcep_msg_initiate{}.
