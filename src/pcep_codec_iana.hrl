
%-- PCEP Messages IANA Numbers -------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#pcep-messages

-define(PCEP_MSG_OPEN,                       1). % RFC5440
-define(PCEP_MSG_KEEPALIVE,                  2). % RFC5440
-define(PCEP_MSG_COMP_REQ,                   3). % RFC5440
-define(PCEP_MSG_COMP_REP,                   4). % RFC5440
-define(PCEP_MSG_NOTIFICATION,               5). % RFC5440
-define(PCEP_MSG_ERROR,                      6). % RFC5440
-define(PCEP_MSG_CLOSE,                      7). % RFC5440
-define(PCEP_MSG_REPORT,                    10). % RFC8231
-define(PCEP_MSG_UPDATE,                    11). % RFC8231
-define(PCEP_MSG_INITIATE,                  12). % RFC8281


%-- PCEP Objects IANA Numbers --------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#pcep-objects

-define(PCEP_OBJC_OPEN,                      1). % RFC5440
-define(PCEP_OBJT_OPEN,                      1). % RFC5440

-define(PCEP_OBJC_RP,                        2). % RFC5440
-define(PCEP_OBJT_RP,                        1). % RFC5440

-define(PCEP_OBJC_NOPATH,                    3). % RFC5440
-define(PCEP_OBJT_NOPATH,                    1). % RFC5440

-define(PCEP_OBJC_ENDPOINT,                  4). % RFC5440
-define(PCEP_OBJT_ENDPOINT_IPV4_ADDR,        1). % RFC5440
-define(PCEP_OBJT_ENDPOINT_IPV6_ADDR,        2). % RFC5440

-define(PCEP_OBJC_BANDWIDTH,                 5). % RFC5440
-define(PCEP_OBJT_BANDWIDTH_REQ,             1). % RFC5440
-define(PCEP_OBJT_BANDWIDTH_TELSP,           2). % RFC5440
-define(PCEP_OBJT_BANDWIDTH_CISCO,           5). % Cisco-specific, not as signed by IANA

-define(PCEP_OBJC_METRIC,                    6). % RFC5440
-define(PCEP_OBJT_METRIC,                    1). % RFC5440

-define(PCEP_OBJC_ERO,                       7). % RFC5440
-define(PCEP_OBJT_ERO,                       1). % RFC5440

-define(PCEP_OBJC_RRO,                       8). % RFC5440
-define(PCEP_OBJT_RRO,                       1). % RFC5440

-define(PCEP_OBJC_LSPA,                      9). % RFC5440
-define(PCEP_OBJT_LSPA,                      1). % RFC5440

-define(PCEP_OBJC_IRO,                      10). % RFC5440
-define(PCEP_OBJT_IRO,                       1). % RFC5440

-define(PCEP_OBJC_SVEC,                     11). % RFC5440
-define(PCEP_OBJT_SVEC,                      1). % RFC5440

-define(PCEP_OBJC_NOTIF,                    12). % RFC5440
-define(PCEP_OBJT_NOTIF,                     1). % RFC5440

-define(PCEP_OBJC_ERROR,                    13). % RFC5440
-define(PCEP_OBJT_ERROR,                     1). % RFC5440

-define(PCEP_OBJC_LOAD_BALANCING,           14). % RFC5440
-define(PCEP_OBJT_LOAD_BALANCING,            1). % RFC5440

-define(PCEP_OBJC_CLOSE,                    15). % RFC5440
-define(PCEP_OBJT_CLOSE,                     1). % RFC5440

-define(PCEP_OBJC_OF,                       21). % RFC5541
-define(PCEP_OBJT_OF,                        1). % RFC5541

-define(PCEP_OBJC_LSP,                      32). % RFC5440
-define(PCEP_OBJT_LSP,                       1). % RFC5440

-define(PCEP_OBJC_SRP,                      33). % RFC8231
-define(PCEP_OBJT_SRP,                       1). % RFC8231

-define(PCEP_OBJC_VENDOR_INFO,              34). % RFC7470
-define(PCEP_OBJT_VENDOR_INFO,               1). % RFC7470

-define(PCEP_OBJC_INTER_LAYER,              36). % RFC8282
-define(PCEP_OBJT_INTER_LAYER,               1). % RFC8282

-define(PCEP_OBJC_SWITCH_LAYER,             37). % RFC8282
-define(PCEP_OBJT_SWITCH_LAYER,              1). % RFC8282

-define(PCEP_OBJC_REQ_ADAP_CAP,             38). % RFC8282
-define(PCEP_OBJT_REQ_ADAP_CAP,              1). % RFC8282

-define(PCEP_OBJC_SERVER_INDICATION,        39). % RFC8282
-define(PCEP_OBJT_SERVER_INDICATION,         1). % RFC8282

-define(PCEP_OBJC_ASSOCIATION,              40). % RFC8697
-define(PCEP_OBJT_ASSOCIATION_IPV4,          1). % RFC8697
-define(PCEP_OBJT_ASSOCIATION_IPV6,          2). % RFC8697


%-- NO-PATH Object NI Field ----------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#no-path-object-ni-field

-define(PCEP_NOPATH_NI_PATH_NOT_FOUND, 0).       % RFC5440
-define(PCEP_NOPATH_NI_PCE_CHAIN_BROKEN, 1).     % RFC5440


%-- PCEP TLVs IANA Numbers -----------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#pcep-tlv-type-indicators

% NO-PATH-VECTOR TLV
-define(PCEP_TLV_NOPATH_VECTOR, 1).             % RFC5440
% OVERLOAD-DURATION TLV
-define(PCEP_TLV_OVERLOAD_DURATION, 2).          % RFC5440
% REQ-MISSING TLV
-define(PCEP_TLV_REQ_MISSING, 3).                % RFC5440
% OF-List
-define(PCEP_TLV_OBJFUN_LIST, 4).                % RFC5541
% VENDOR-INFORMATION-TLV
-define(PCEP_TLV_VENDOR_INFO, 7).                % RFC7470
% STATEFUL-PCE-CAPABILITY
-define(PCEP_TLV_STATEFUL_PCE_CAP, 16).          % RFC8231
% SYMBOLIC-PATH-NAME
-define(PCEP_TLV_SYMBOLIC_PATH_NAME, 17).        % RFC8231
% IPV4-LSP-IDENTIFIERS
-define(PCEP_TLV_IPV4_LSP_ID, 18).               % RFC8231
% IPV6-LSP-IDENTIFIERS
-define(PCEP_TLV_IPV6_LSP_ID, 19).               % RFC8231
% LSP-ERROR-CODE
-define(PCEP_TLV_LSP_ERROR_CODE, 20).            % RFC8231
% RSVP-ERROR-SPEC
-define(PCEP_TLV_RSVP_ERROR_SPEC, 21).           % RFC8231
% LSP-DB-VERSION
-define(PCEP_TLV_LSP_DB_VERSION, 23).            % RFC8232
% SPEAKER-ENTITY-ID
-define(PCEP_TLV_SPEAKER_ENTITY_ID, 24).         % RFC8232
% SR-PCE-CAPABILITY
-define(PCEP_TLV_SR_PCE_CAP, 26).                % RFC8664 (DEPRECATED)
% PATH-SETUP-TYPE
-define(PCEP_TLV_PATH_SETUP_TYPE, 28).           % RFC8408
% Operator-configured Association Range
-define(PCEP_TLV_OP_CONFIGURED_ASSOC_RANGE, 29). % RFC8697
% Global Association Source
-define(PCEP_TLV_GLOBAL_ASSOC_SOURCE, 30).       % RFC8697
% Extended Association ID
-define(PCEP_TLV_EXT_ASSOC_ID, 31).              % RFC8697
% PATH-SETUP-TYPE-CAPABILITY
-define(PCEP_TLV_PATH_SETUP_TYPE_CAP, 34).       % RFC8408
% ASSOC-Type-List
-define(PCEP_TLV_ASSOC_TYPE_LIST, 35).           % RFC8697
% Custom CISCO TLV to bind a label
-define(PCEP_TLV_CISCO_BINDING_LABEL, 65505).
% Arbirtrary TLV
-define(PCEP_TLV_ARBITRARY, 65533).


%-- PCEP Setup Type TLV's Sub-TLVs IANA Numbers --------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#path-setup-type-capability-sub-tlv-type-indicators

-define(PCEP_TLV_PATH_SETUP_TYPE_CAP_SR, 26).    % RFC8664


%-- PCEP Metrics IANA Numbers --------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#metric-object-t-field

% IGP metric
-define(PCEP_METRIC_IGP, 1).                     % RFC5440
% TE metric
-define(PCEP_METRIC_TE, 2).                      % RFC5440
% Hop Counts
-define(PCEP_METRIC_HOP_COUNT, 3).               % RFC5440
% Aggregate bandwidth consumption
-define(PCEP_METRIC_AGGREGATE_BW, 4).            % RFC5541
% Load of the most loaded link
-define(PCEP_METRIC_MOST_LOADED_LINK, 5).        % RFC5541
% Cumulative IGP cost
-define(PCEP_METRIC_CUMULATIVE_IGP, 6).          % RFC5541
% Cumulative TE cost
-define(PCEP_METRIC_CUMULATIVE_TE, 7).           % RFC5541
% Segment-ID (SID) Depth
-define(PCEP_METRIC_SEGMENT_ID_DEPTH, 11).       % RFC8664


%-- RSVP ERO SubObject Type IANA Numbers ---------------------------------------
% https://www.iana.org/assignments/rsvp-parameters/rsvp-parameters.xhtml#rsvp-parameters-25

% IPv4 prefix
-define(RSVP_ERO_SUBOBJ_TYPE_IPV4, 1).           % RFC3209
% IPv6 prefix
-define(RSVP_ERO_SUBOBJ_TYPE_IPV6, 2).           % RFC3209
% Label
-define(RSVP_ERO_SUBOBJ_TYPE_LABEL, 3).          % RFC3473
% Unnumbered Interface ID
-define(RSVP_ERO_SUBOBJ_TYPE_UNUM, 4).           % RFC3477
% Autonomous system number
-define(RSVP_ERO_SUBOBJ_TYPE_ASN, 32).           % RFC3209
% SR-ERO
-define(RSVP_ERO_SUBOBJ_TYPE_SR, 36).            % RFC8664


%-- PCEP SR-ERO NAI Types ---------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#pcep-sr-ero-nai-types

% NAI is absent.
-define(PCEP_SR_SUBOBJ_NAI_ABSENT, 0).           % RFC8664
% NAI is an IPv4 node ID.
-define(PCEP_SR_SUBOBJ_NAI_IPV4_NODE, 1).        % RFC8664
% NAI is an IPv6 node ID.
-define(PCEP_SR_SUBOBJ_NAI_IPV6_NODE, 2).        % RFC8664
% NAI is an IPv4 adjacency.
-define(PCEP_SR_SUBOBJ_NAI_IPV4_ADJACENCY, 3).   % RFC8664
% NAI is an IPv6 adjacency with global IPv6 addresses.
-define(PCEP_SR_SUBOBJ_NAI_IPV6_ADJACENCY, 4).   % RFC8664
% NAI is an unnumbered adjacency with IPv4 node IDs.
-define(PCEP_SR_SUBOBJ_NAI_UNNUMBERED_IPV4_ADJACENCY, 5).       % RFC8664
% NAI is an IPv6 adjacency with link-local IPv6 addresses.
-define(PCEP_SR_SUBOBJ_NAI_LINKLOCAL_IPV6_ADJACENCY, 6).        % RFC8664


%-- PCEP Notification IANA Numbers --------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#notification-object

-define(PCEP_NOTT_REQ_CANCELED,              1). % RFC5440
-define(PCEP_NOTV_PCC_CANCELED_REQ,          1). % RFC5440
-define(PCEP_NOTV_PCE_CANCELED_REQ,          2). % RFC5440

-define(PCEP_NOTT_PCE_CONGESTION,            2). % RFC5440
-define(PCEP_NOTV_PCE_CONGESTED,             1). % RFC5440
-define(PCEP_NOTV_PCE_RECOVERED,             2). % RFC5440

-define(PCEP_NOTT_PCE_LIMIT_EXCEEDED,        4). % RFC8231
-define(PCEP_NOTV_PCE_LIMIT_EXCEEDED,        1). % RFC8231


%-- PCEP Errors IANA Numbers ---------------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#pcep-error-object

% PCEP session establishment failure
-define(PCEP_ERRT_SESSION_FAILURE, 1).                       % RFC5440
% reception of an invalid Open message or a non Open message
-define(PCEP_ERRV_RECVD_INVALID_OPEN_MSG, 1).                % RFC5440
% no Open message received before the expiration of the OpenWait timer
-define(PCEP_ERRV_OPENWAIT_TIMED_OUT, 2).                    % RFC5440
% unacceptable and non negotiable session characteristics
-define(PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NO_NEG, 3).          % RFC5440
% unacceptable but negotiable session characteristics
-define(PCEP_ERRV_UNACCEPTABLE_OPEN_MSG_NEG, 4).             % RFC5440
% reception of a second Open message with still unacceptable session characteristics
-define(PCEP_ERRV_RECVD_SECOND_OPEN_MSG_UNACCEPTABLE, 5).    % RFC5440
% reception of a PCErr message proposing unacceptable session characteristics
-define(PCEP_ERRV_RECVD_PCERR, 6).                           % RFC5440
% No Keepalive or PCErr message received before the expiration of the KeepWait timer
-define(PCEP_ERRV_KEEPALIVEWAIT_TIMED_OUT, 7).               % RFC5440
% PCEP version not supported
-define(PCEP_ERRV_PCEP_VERSION_NOT_SUPPORTED, 8).            % RFC5440

% Capability not supported
-define(PCEP_ERRT_CAPABILITY_NOT_SUPPORTED, 2).              % RFC5440

% Unknown Object
-define(PCEP_ERRT_UNKNOW_OBJECT, 3).                         % RFC5440
% Unrecognized object class
-define(PCEP_ERRV_UNREC_OBJECT_CLASS, 1).                    % RFC5440
% Unrecognized object Type
-define(PCEP_ERRV_UNREC_OBJECT_TYPE, 2).                     % RFC5440

% Not supported object
-define(PCEP_ERRT_NOT_SUPPORTED_OBJECT, 4).                  % RFC5440
% Not supported object class
-define(PCEP_ERRV_NOT_SUPPORTED_OBJECT_CLASS, 1).            % RFC5440
% Not supported object Type
-define(PCEP_ERRV_NOT_SUPPORTED_OBJECT_TYPE, 2).             % RFC5440

% Policy violation
-define(PCEP_ERRT_POLICY_VIOLATION, 5).                      % RFC5440
% C bit of the METRIC object set (request rejected)
-define(PCEP_ERRV_C_BIT_SET_IN_METRIC_OBJECT, 1).            % RFC5440
% O bit of the RP object cleared (request rejected)
-define(PCEP_ERRV_O_BIT_CLEARD_IN_RP_OBJECT, 2).             % RFC5440
% objective function not allowed (request rejected)
-define(PCEP_ERRV_OBJFUN_NOT_ALLOWED, 3).            % RFC5541
% OF bit of the RP object set (request rejected)
-define(PCEP_ERRV_RP_OF_BIT_SET, 4).                         % RFC5541

% Mandatory Object missing
-define(PCEP_ERRT_MANDATORY_OBJECT_MISSING, 6).              % RFC5440
% RP object missing
-define(PCEP_ERRV_RP_OBJECT_MISSING, 1).                     % RFC5440
% RRO missing for a reoptimization request (R bit of the RP object set
-define(PCEP_ERRV_RRO_OBJECT_MISSING_FOR_REOP, 2).           % RFC5440
% END-POINTS object missing
-define(PCEP_ERRV_EP_OBJECT_MISSING, 3).                     % RFC5440
% LSP object missing
-define(PCEP_ERRV_LSP_OBJECT_MISSING, 8).                    % RFC8231
% ERO object missing
-define(PCEP_ERRV_ERO_OBJECT_MISSING, 9).                    % RFC8231
% SRP object missing
-define(PCEP_ERRV_SRP_OBJECT_MISSING, 10).                   % RFC8231
% LSP-IDENTIFIERS TLV missing
-define(PCEP_ERRV_LSP_ID_TLV_MISSING, 11).                   % RFC8231
% LSP-DB-VERSION TLV missing
-define(PCEP_ERRV_LSP_DB_TLV_MISSING, 12).                   % RFC8232

% Synchronized path computation request missing
-define(PCEP_ERRT_SYNC_PC_REQ_MISSING, 7).                   % RFC5440

% Unknown request reference
-define(PCEP_ERRT_UNKNOWN_REQ_REF, 8).                       % RFC5440

% Attempt to establish a second PCEP session
-define(PCEP_ERRT_SECOND_PCEP_SESSION_ATTEMPTED, 9).         % RFC5440

% Reception of an invalid object
-define(PCEP_ERRT_INVALID_OBJECT, 10).                       % RFC5440
% Reception of an object with P flag not set although the P-flag must be set according to this specification
-define(PCEP_ERRV_P_FLAG_NOT_CORRECT_IN_OBJECT, 1).          % RFC5440
% Bad label value
-define(PCEP_ERRV_BAD_LABEL_VALUE, 2).                       % RFC8664
% Unsupported number of SR-ERO subobjects
-define(PCEP_ERRV_UNSUPPORTED_NUM_SR_ERO_SUBOBJECTS, 3).     % RFC8664
% Bad label format
-define(PCEP_ERRV_BAD_LABEL_FORMAT, 4).                      % RFC8664
% ERO mixes SR-ERO subobjects with other subobject types
-define(PCEP_ERRV_SR_ERO_MIXED, 5).                          % RFC8664
% Both SID and NAI are absent in the SR-ERO subobject
-define(PCEP_ERRV_SR_ERO_SID_NAI_ABSENT, 6).                 % RFC8664
% Both SID and NAI are absent in the SR-RRO subobject
-define(PCEP_ERRV_SR_RRO_SID_NAI_ABSENT, 7).                 % RFC8664
% SYMBOLIC-PATH-NAME TLV missing
-define(PCEP_ERRV_SYMBOLIC_PATH_NAME_MISSING, 8).            % RFC8281
% MSD exceeds the default for the PCEP session
-define(PCEP_ERRV_MSD_EXCEEDS_DEFAULT, 9).                   % RFC8664
% RRO mixes SR-RRO subobjects with other subobject types
-define(PCEP_ERRV_SR_RRO_MIXED, 10).                         % RFC8664
% Malformed object
-define(PCEP_ERRV_MALFORMED_OBJECT, 11).                     % RFC8408
% Missing PCE-SR-CAPABILITY sub-TLV
-define(PCEP_ERRV_SR_CAP_TLV_MISSING, 12).                   % RFC8664
% Unsupported NAI Type in the SR-ERO/SR-RRO subobject
-define(PCEP_ERRV_UNSUPPORTED_NAI_TYPE, 13).                 % RFC8664
% Unknown SID
-define(PCEP_ERRV_UNKNOWN_SID, 14).                          % RFC8664
% NAI cannot be resolved to a SID
-define(PCEP_ERRV_CANNOT_RESOLVE_NAI, 15).                   % RFC8664
% Could not find SRGB
-define(PCEP_ERRV_SRGB_NOT_FOUND, 16).                       % RFC8664
% SID index exceeds SRGB size
-define(PCEP_ERRV_SID_EXCEEDS_SRGB_SIZE, 17).                % RFC8664
% Could not find SRLB
-define(PCEP_ERRV_SRLB_NOT_FOUND, 18).                       % RFC8664
% SID index exceeds SRLB size
-define(PCEP_ERRV_SID_EXCEEDS_SRLB_SIZE, 19).                % RFC8664
% Inconsistent SIDs in SR-ERO / SR-RRO subobjects
-define(PCEP_ERRV_INCONSISTENT_SIDS, 20).                    % RFC8664
% MSD must be nonzero
-define(PCEP_ERRV_MSD_MUST_BE_NONZERO, 21).                  % RFC8664

% Invalid Operation
-define(PCEP_ERRT_INVALID_OPERATION, 19).                    % RFC8231
% Attempted LSP Update Request for a non-delegated LSP.
% The PCEP-ERROR object is followed by the LSP object that identifies the LSP
-define(PCEP_ERRV_LSP_UPDATE_FOR_NON_DELEGATED_LSP, 1).      % RFC8231
% Attempted LSP Update Request if the stateful PCE capability was not advertised
-define(PCEP_ERRV_LSP_UPDATE_NON_ADVERTISED_PCE, 2).         % RFC8231
% Attempted LSP Update Request for an LSP identified by an unknown PLSP-ID
-define(PCEP_ERRV_LSP_UPDATE_UNKNOWN_PLSP_ID, 3).            % RFC8231
% Attempted LSP State Report if active stateful PCE capability was not advertised
-define(PCEP_ERRV_LSP_REPORT_NON_ADVERTISED_PCE, 5).         % RFC8231
% PCE-initiated LSP limit reached
-define(PCEP_ERRV_PCE_INIT_LSP_LIMIT_REACHED, 6).            % RFC8281
% Delegation for PCE-initiated LSP cannot be revoked
-define(PCEP_ERRV_PCE_INIT_LSP_DELEGATION_CANT_REVOKE, 7).   % RFC8281
% Non-zero PLSP-ID in LSP Initiate Request
-define(PCEP_ERRV_LSP_INIT_NON_ZERO_PLSP_ID, 8).             % RFC8281
% LSP is not PCE initiated
-define(PCEP_ERRV_LSP_NOT_PCE_INITIATED, 9).                 % RFC8281
% PCE-initiated operation-frequency limit reached
-define(PCEP_ERRV_PCE_INIT_OP_FREQ_LIMIT_REACHED, 10).       % RFC8281

% LSP State Synchronization Error
-define(PCEP_ERRT_LSP_STATE_SYNC_ERROR, 20).                 % RFC8231
% A PCE indicates to a PCC that it cannot process (an otherwise valid) LSP State Report.
% The PCEP- ERROR object is followed by the LSP object that identifies the LSP
-define(PCEP_ERRV_PCE_CANT_PROCESS_LSP_REPORT, 1).           % RFC8231
% LSP-DB version mismatch
-define(PCEP_ERRV_LSP_DB_VERSION_MISMATCH, 2).               % RFC8232
% Attempt to trigger synchronization before PCE trigger
-define(PCEP_ERRV_TRIGGER_ATTEMPT_BEFORE_PCE_TRIGGER, 3).    % RFC8232
% Attempt to trigger a synchronization when the PCE triggered synchronization capability has not been advertised
-define(PCEP_ERRV_TRIGGER_ATTEMPT_NO_PCE_TRIGGER_CAP, 4).    % RFC8232
% A PCC indicates to a PCE that it cannot complete the State Synchronization
-define(PCEP_ERRV_PCC_CANT_COMPLETE_STATE_SYNC, 5).          % RFC8231
% Received an invalid LSP-DB Version Number
-define(PCEP_ERRV_INVALID_LSP_DB_VERSION_NUMBER, 6).         % RFC8232
% Received an invalid Speaker Entity Identifier
-define(PCEP_ERRV_INVALID_SPEAKER_ENTITY_ID, 7).             % RFC8232

% Invalid traffic engineering path setup type
-define(PCEP_ERRT_INVALID_TE_PATH_SETUP_TYPE, 21).           % RFC8408
% Unsupported path setup type
-define(PCEP_ERRV_UNSUPPORTED_PATH_SETUP_TYPE, 1).           % RFC8408
% Mismatched path setup type
-define(PCEP_ERRV_MISMATCHED_PATH_SETUP_TYPE, 2).            % RFC8408

% Bad parameter value
-define(PCEP_ERRT_BAD_PARAMETER_VALUE, 23).                  % RFC8281
% SYMBOLIC-PATH-NAME in use
-define(PCEP_ERRV_SYMBOLIC_PATH_NAME_IN_USE, 1).             % RFC8281
% Speaker identity included for an LSP that is not PCE initiated
-define(PCEP_ERRV_LSP_SPEAKER_ID_NOT_PCE_INITIATED, 2).      % RFC8281

% LSP instantiation error
-define(PCEP_ERRT_LSP_INSTANTIATE_ERROR, 24).                % RFC8281
% Unacceptable instantiation parameters
-define(PCEP_ERRV_UNACCEPTABLE_INSTANTIATE_ERROR, 1).        % RFC8281
% Internal error
-define(PCEP_ERRV_INTERNAL_ERROR, 2).                        % RFC8281
% Signaling error
-define(PCEP_ERRV_SIGNALLING_ERROR, 3).                      % RFC8281

% Association Error
-define(PCEP_ERRT_ASSOCIATION_ERROR, 26).                    % RFC8697
% Association Type is not supported
-define(PCEP_ERRV_ASSOC_TYPE_NOT_SUPPORTED, 1).              % RFC8697
% Too many LSPs in the association group
-define(PCEP_ERRV_TOO_MANY_LSPS_IN_ASSOC_GRP, 2).            % RFC8697
% Too many association groups
-define(PCEP_ERRV_TOO_MANY_ASSOC_GROUPS, 3).                 % RFC8697
% Association unknown
-define(PCEP_ERRV_ASSOCIATION_UNKNOWN, 4).                   % RFC8697
% Operator-configured association information mismatch
-define(PCEP_ERRV_OP_CONF_ASSOC_INFO_MISMATCH, 5).           % RFC8697
% Association information mismatch
-define(PCEP_ERRV_ASSOC_INFO_MISMATCH, 6).                   % RFC8697
% Cannot join the association group
-define(PCEP_ERRV_CANNOT_JOIN_ASSOC_GROUP, 7).               % RFC8697
% Association ID not in range
-define(PCEP_ERRV_ASSOC_ID_NOT_IN_RANGE, 8).                 % RFC8697

% Error value for error types that don't have any:
%   * PCEP_ERRT_CAPABILITY_NOT_SUPPORTED
%   * PCEP_ERRT_SYNC_PC_REQ_MISSING
%   * PCEP_ERRT_UNKNOWN_REQ_REF
%   * PCEP_ERRT_SECOND_PCEP_SESSION_ATTEMPTED
-define(PCEP_ERRV_UNASSIGNED, 0).


%-- PCEP Close Reasons IANA Numbers --------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#close-object-reason-field

%  No explanation provided
-define(PCEP_CLOSE_REASON_NO_REASON, 1).                     % RFC5440
%  DeadTimer expired
-define(PCEP_CLOSE_REASON_DEADTIMER, 2).                     % RFC5440
%  Reception of a malformed PCEP message
-define(PCEP_CLOSE_REASON_FORMAT, 3).                        % RFC5440
%  Reception of an unacceptable number of unknown requests/replies
-define(PCEP_CLOSE_REASON_TOO_MUCH_BAD_REQ, 4).              % RFC5440
%  Reception of an unacceptable number of unrecognized PCEP messages
-define(PCEP_CLOSE_REASON_TOO_MUCH_BAD_MSG, 5).              % RFC5440


%-- PCEP LSP Error Codes IANA Numbers ------------------------------------------
% https://www.iana.org/assignments/pcep/pcep.xhtml#lsp-error-code-tlv-error-code-field
% Unknown reason
-define(PCEP_LSP_ERROR_UNKNOWN, 1).                          % RFC8231
% Limit reached for PCE-controlled LSPs
-define(PCEP_LSP_ERROR_LIMIT_REACHED, 2).                    % RFC8231
% Too many pending LSP update requests
-define(PCEP_LSP_ERROR_TOO_MANY_PENDING_UPDATES, 3).         % RFC8231
% Unacceptable parameters
-define(PCEP_LSP_ERROR_UNACCEPTABLE_PARAMETERS, 4).          % RFC8231
% Internal error
-define(PCEP_LSP_ERROR_INTERNAL_ERROR, 5).                   % RFC8231
% LSP administratively brought down
-define(PCEP_LSP_ERROR_ADMIN_BROUGHT_DOWN, 6).               % RFC8231
% LSP preempted
-define(PCEP_LSP_ERROR_PREEMPTED, 7).                        % RFC8231
% RSVP signaling error
-define(PCEP_LSP_ERROR_RSVP_SIGNALING_ERROR, 8).             % RFC8231


%-- Path Setup Type Codes, NOT DEFINED BY IANA ?!? -----------------------------
% Path is set up using the RSVP-TE signaling protocol
% https://tools.ietf.org/html/rfc8408#section-3
-define (PCEP_PST_RSVPTE, 0).                                % RFC8408
% Traffic-engineering path is set up using Segment Routing
% https://tools.ietf.org/html/rfc8664#section-4.1.2
-define (PCEP_PST_SRTE, 1).                                  % RFC8664


%-- LSP Operational State, NOT DEFINED BY IANA ---------------------------------
% https://tools.ietf.org/html/rfc8231#section-7.3
% Not active
-define(PCEP_OPSTATUS_DOWN, 0).                              % RFC8231
% Signaled
-define(PCEP_OPSTATUS_UP, 1).                                % RFC8231
% Up and carrying traffic
-define(PCEP_OPSTATUS_ACTIVE, 2).                            % RFC8231
% LSP is being torn down, and resources are being released
-define(PCEP_OPSTATUS_GOING_DOWN, 3).                        % RFC8231
% LSP is being signaled
-define(PCEP_OPSTATUS_GOING_UP, 4).                          % RFC8231
