-module(pcep_codec_specs).

-on_load(init/0).


%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("pcep_codec.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([get/1]).


%%% IMPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-import(codec_sequencer_spec, [loop/1]).
-import(codec_sequencer_spec, [group/1]).
-import(codec_sequencer_spec, [object/3, object/4]).
-import(codec_sequencer_spec, [record/3, record/4]).
-import(codec_sequencer_spec, [map/2, map/3, map/4]).
-import(codec_sequencer_spec, [list/2, list/3]).
-import(codec_sequencer_spec, [tuple/2, tuple/3]).
-import(codec_sequencer_spec, [codec/2, codec/3]).
-import(codec_sequencer_spec, [option/2]).


%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-- PCEP Messages Specifications -----------------------------------------------

% https://tools.ietf.org/html/rfc5440#section-6.2
-define(PCEP_MSG_OPEN_SPEC,
    record(required, #pcep_msg_open{}, [
        codec(required, open, #pcep_msg_open.open)                     % RFC5440
    ])
).

% https://tools.ietf.org/html/rfc5440#section-6.4
% https://tools.ietf.org/html/rfc8231#section-6.4
% https://tools.ietf.org/html/rfc5541#section-3.2
% https://tools.ietf.org/html/rfc8282#section-5
% https://tools.ietf.org/html/rfc7470#section-2
% https://tools.ietf.org/html/rfc8697#section-6.3.2
-define(PCEP_MSG_COMPREQ_SPEC,
    record(required, #pcep_msg_compreq{}, [                            % RFC5440
        % list(optional, #pcep_msg_compreq.svecs, loop(
        %     record(required, #pcep_reqvec{}, [
        %         codec(required, svec, #pcep_reqvec.svec),              % RFC5440
        %         codec(optional, objfun, #pcep_reqvec.objfun),          % RFC5541
        %         list(optional, #pcep_reqvec.metrics, loop(
        %             codec(required, metric)                            % RFC????
        %         )),
        %         list(optional, #pcep_reqvec.vendors, loop(
        %             codec(required, vendor_info)                       % RFC7470
        %         ))
        % )),
        list(required, #pcep_msg_compreq.request_list, loop(
            record(required, #pcep_compreq{}, [
                codec(required, rp, #pcep_compreq.rp),                 % RFC5440
                % list(optional, #pcep_compreq.vendors, loop(
                %     codec(required, vendor_info)                       % RFC7470
                % )),
                list(required, #pcep_compreq.endpoints, loop(
                    record(required, #pcep_endpoint{}, [
                        codec(required, endpoint,
                          #pcep_endpoint.endpoint),                    % RFC7470
                        list(optional, #pcep_endpoint.paths, loop(
                            record(required, #pcep_rpath{}, [
                                codec(required, rro,                   % RFC7470
                                  #pcep_rpath.rro),
                                codec(optional, bandwidth,             % RFC7470
                                  #pcep_rpath.bandwidth)%,
                                % list(optional,
                                %   #pcep_endpoint_rro.vendors, loop(
                                %     codec(required, vendor_info)       % RFC7470
                                % ))
                            ])
                        ))
                    ])
                )),
                codec(optional, lsp, #pcep_compreq.lsp),               % RFC8231
                codec(optional, lspa, #pcep_compreq.lspa),             % RFC5440
                codec(optional, bandwidth, #pcep_compreq.bandwidth),   % RFC5440
                list(optional, #pcep_compreq.metrics, loop(
                    codec(required, metric)                            % RFC5440
                )),
                % list(optional, #pcep_compreq.associations, loop(
                %     codec(required, association),% RFC8697 May go after the OF ?
                % )),
                % list(optional, #pcep_compreq.objfuns, loop(
                %     codec(required, objfun),                  % RFC5541, RFC8282
                % )),
                record(optional, #pcep_rpath{}, #pcep_compreq.rpath, [
                    codec(required, rro, #pcep_rpath.rro),             % RFC5440
                    codec(optional, bandwidth,                         % RFC7470
                      #pcep_rpath.bandwidth)
                ]),
                codec(optional, iro, #pcep_compreq.iro),               % RFC5440
                codec(optional, load_balancing,                        % RFC5440
                  #pcep_compreq.load_balancing)%,
                % group([
                %     codec(required, inter_layer,                       % RFC8282
                %       #pcep_compreq.inter_layer),
                %     codec(optional, switch_layer,                      % RFC8282
                %       #pcep_compreq.switch_layer),
                % ]),
                % codec(optional, req_adap_cap,                          % RFC8282
                %       #pcep_compreq.req_adap_cap)

            ])
        ))
    ])
).

% https://tools.ietf.org/html/rfc5440#section-6.5
% https://tools.ietf.org/html/rfc8231#section-6.5
% https://tools.ietf.org/html/rfc5541#section-3.2 (without SVEC)
% https://tools.ietf.org/html/rfc8282#section-5
% https://tools.ietf.org/html/rfc7470#section-2
% https://tools.ietf.org/html/rfc8697#section-6.3.3
-define(PCEP_MSG_COMPREP_SPEC,
    record(required, #pcep_msg_comprep{}, [
        list(required, #pcep_msg_comprep.reply_list, loop(
            record(required, #pcep_comprep{}, [
                codec(required, rp, #pcep_comprep.rp),                 % RFC5440
                % list(optional, #pcep_path.vendors, loop(
                %     codec(required, vendor_info)                       % RFC7470
                % )),
                codec(optional, lsp, #pcep_comprep.lsp),               % RFC8231
                codec(optional, nopath, #pcep_comprep.nopath),         % RFC5440
                % list(optional, #pcep_comprep.associations, loop(
                %     codec(required, association),                      % RFC8697
                % )),
                % list(optional, #pcep_comprep.objfuns, loop(
                %     codec(required, objfun),                  % RFC5541, RFC8282
                % )),
                codec(optional, lspa, #pcep_comprep.lspa),             % RFC5440
                codec(optional, bandwidth, #pcep_comprep.bandwidth),   % RFC5440
                list(optional, #pcep_comprep.metrics, loop(
                    codec(required, metric)                            % RFC5440
                )),
                codec(optional, iro, #pcep_comprep.iro),               % RFC5440
                % codec(optional, inter_layer,
                %   #pcep_comprep.inter_layer),                          % RFC8282
                % codec(optional, switch_layer,
                %   #pcep_comprep.switch_layer),                         % RFC8282
                % codec(optional, req_adap_cap,
                %   #pcep_comprep.req_adap_cap),                         % RFC8282
                % codec(optional, server_indication,
                %   #pcep_comprep.server_indication),                    % RFC8282
                list(optional, #pcep_comprep.endpoints, loop(
                    record(required, #pcep_endpoint{}, [
                        codec(optional, endpoint,                      % RFC7470 ?
                          #pcep_endpoint.endpoint),
                        list(optional, #pcep_endpoint.paths, loop(
                            record(required, #pcep_path{}, [
                                codec(required, ero, #pcep_path.ero),  % RFC5440
                                % list(optional, #pcep_comprep.objfuns, loop(
                                %     codec(required, objfun),  % RFC5541, RFC8282
                                % )),
                                % list(optional, #pcep_path.vendors, loop(
                                %     codec(required, vendor_info)       % RFC7470
                                % )),
                                codec(optional, lspa,                  % RFC5440
                                  #pcep_path.lspa),
                                codec(optional, bandwidth,             % RFC5440
                                  #pcep_path.bandwidth),
                                list(optional, #pcep_path.metrics, loop(
                                    codec(required, metric)            % RFC5440
                                )),
                                codec(optional, iro, #pcep_path.iro)   % RFC5440
                                % codec(optional, inter_layer,           % RFC8282
                                %   #pcep_path.inter_layer),
                                % codec(optional, switch_layer,          % RFC8282
                                %   #pcep_path.switch_layer),
                                % codec(optional, req_adap_cap,          % RFC8282
                                %   #pcep_path.req_adap_cap),
                                % codec(optional, server_indication,     % RFC8282
                                %   #pcep_path.server_indication),
                            ])
                        ))
                    ])
                ))
            ])
        ))
    ])
).

% https://tools.ietf.org/html/rfc5440#section-6.6
-define(PCEP_MSG_NOTIF_SPEC,
    record(required, #pcep_msg_notif{}, [
        list(required, #pcep_msg_notif.notif_list, loop(
            record(required, #pcep_notif{}, [
                list(optional, #pcep_notif.id_list, loop(
                    codec(required, rp)                                % RFC5440
                )),
                list(optional, #pcep_notif.notifications, loop(
                    codec(required, notif)                             % RFC5440
                ))
            ])
        ))
    ])
).

% https://tools.ietf.org/html/rfc5440#section-6.7
% https://tools.ietf.org/html/rfc8231#section-6.3
-define(PCEP_MSG_ERROR_SPEC,
    record(required, #pcep_msg_error{}, [
        list(required, #pcep_msg_error.error_list, loop(
            record(required, #pcep_error{}, [
                option(optional, [
                    list(optional, #pcep_error.id_list, loop(
                        codec(required, rp)                            % RFC5440
                    )),
                    list(optional, #pcep_error.id_list, loop(
                        codec(required, srp)                           % RFC8231
                    ))
                ]),
                list(required, #pcep_error.errors, loop(
                    codec(required, error)                             % RFC5440
                )),
                codec(optional, open, #pcep_error.open)                % RFC5440
            ])
        ))
    ])
).

% https://tools.ietf.org/html/rfc5440#section-6.8
-define(PCEP_MSG_CLOSE_SPEC,
    record(required, #pcep_msg_close{}, [
        codec(required, close, #pcep_msg_close.close)                  % RFC5440
    ])
).

% https://tools.ietf.org/html/rfc8231#section-6.1
% https://tools.ietf.org/html/rfc8697#section-6.3.1
-define(PCEP_MSG_REPORT_SPEC,
    record(required, #pcep_msg_report{}, [
        list(required, #pcep_msg_report.report_list, loop(
            record(required, #pcep_report{}, [
                codec(optional, srp, #pcep_report.srp),                 % RFC8231
                codec(required, lsp, #pcep_report.lsp),                 % RFC8231
                % list(optional, #pcep_report.associations, loop(
                %     codec(required, association),                      % RFC8697
                % )),
                codec(required, ero, #pcep_report.ero),                % RFC8231
                group([
                    codec(optional, bandwidth,
                      #pcep_report.actual_bandwidth),                  % RFC8231
                    list(optional, #pcep_report.actual_metrics, loop(
                        codec(required, metric)                        % RFC8231
                    )),
                    codec(required, rro, #pcep_report.rro)             % RFC8231
                ]),
                codec(optional, lspa, #pcep_report.lspa),              % RFC5440
                codec(optional, bandwidth, #pcep_report.bandwidth),    % RFC5440
                list(optional, #pcep_report.metrics, loop(
                    codec(required, metric)                            % RFC5440
                )),
                codec(optional, iro, #pcep_report.iro)                 % RFC5440
            ])
        ))
    ])
).

% https://tools.ietf.org/html/rfc8231#section-6.2
% https://tools.ietf.org/html/rfc8697#section-6.3.1
-define(PCEP_MSG_UPDATE_SPEC,
    record(required, #pcep_msg_update{}, [
        list(required, #pcep_msg_update.update_list, loop(
            record(required, #pcep_update{}, [
                codec(required, srp, #pcep_update.srp),                % RFC8231
                codec(required, lsp, #pcep_update.lsp),                % RFC8231
                codec(required, ero, #pcep_update.ero),                % RFC8231
                % list(optional, #pcep_update.associations, loop(
                %     codec(required, association),                      % RFC8697
                % )),
                codec(optional, bandwidth, #pcep_update.bandwidth),    % RFC8231
                list(optional, #pcep_update.metrics, loop(
                    codec(required, metric)                            % RFC8231
                ))
            ])
        ))
    ])
).

% https://tools.ietf.org/html/rfc8281#section-5.1
% https://tools.ietf.org/html/rfc8697#section-6.3.1
-define(PCEP_MSG_INITIATE_SPEC,
    record(required, #pcep_msg_initiate{}, [
        list(required, #pcep_msg_initiate.action_list, loop(
            option(required, [
                record(required, #pcep_lsp_init{}, [
                    codec(required, srp, #pcep_lsp_init.srp),          % RFC8281
                    codec(required, lsp, #pcep_lsp_init.lsp),          % RFC8281
                    codec(optional, endpoint, #pcep_lsp_init.endpoint),% RFC8281
                    codec(required, ero, #pcep_lsp_init.ero),          % RFC8281
                    % list(optional, #pcep_initiate.associations, loop(
                    %     codec(required, association),                  % RFC8697
                    % )),
                    % list(optional, #pcep_initiate.objfuns, loop(
                    %     codec(required, objfun),                       % RFC5541
                    % )),
                    codec(optional, lspa, #pcep_lsp_init.lspa),        % RFC5440
                    codec(optional, bandwidth,                         % RFC5440
                      #pcep_lsp_init.bandwidth),
                    list(optional, #pcep_lsp_init.metrics, loop(
                        codec(required, metric)                        % RFC5440
                    )),
                    codec(optional, iro, #pcep_lsp_init.iro)           % RFC5440
                ]),
                record(required, #pcep_lsp_delete{}, [
                    codec(required, srp, #pcep_lsp_delete.srp),        % RFC8281
                    codec(required, lsp, #pcep_lsp_delete.lsp)         % RFC8281
                ])
            ])
        ))
    ])
).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(open) -> persistent_term:get({?MODULE, msg_open_spec});
get(compreq) -> persistent_term:get({?MODULE, msg_compreq_spec});
get(comprep) -> persistent_term:get({?MODULE, msg_comprep_spec});
get(notif) -> persistent_term:get({?MODULE, msg_notif_spec});
get(error) -> persistent_term:get({?MODULE, msg_error_spec});
get(close) -> persistent_term:get({?MODULE, msg_close_spec});
get(report) -> persistent_term:get({?MODULE, msg_report_spec});
get(update) -> persistent_term:get({?MODULE, msg_update_spec});
get(initiate) -> persistent_term:get({?MODULE, msg_initiate_spec}).


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    persistent_term:put({?MODULE, msg_open_spec}, ?PCEP_MSG_OPEN_SPEC),
    persistent_term:put({?MODULE, msg_compreq_spec}, ?PCEP_MSG_COMPREQ_SPEC),
    persistent_term:put({?MODULE, msg_comprep_spec}, ?PCEP_MSG_COMPREP_SPEC),
    persistent_term:put({?MODULE, msg_notif_spec}, ?PCEP_MSG_NOTIF_SPEC),
    persistent_term:put({?MODULE, msg_error_spec}, ?PCEP_MSG_ERROR_SPEC),
    persistent_term:put({?MODULE, msg_close_spec}, ?PCEP_MSG_CLOSE_SPEC),
    persistent_term:put({?MODULE, msg_report_spec}, ?PCEP_MSG_REPORT_SPEC),
    persistent_term:put({?MODULE, msg_update_spec}, ?PCEP_MSG_UPDATE_SPEC),
    persistent_term:put({?MODULE, msg_initiate_spec}, ?PCEP_MSG_INITIATE_SPEC),
    ok.

