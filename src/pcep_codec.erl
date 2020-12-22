-module(pcep_codec).

% * [RFC 5440](https://tools.ietf.org/html/rfc5440) PCEP Protocol
% * [RFC 8231](https://tools.ietf.org/html/rfc8231) Extensions for Stateful PCE
% * [RFC 8232](https://tools.ietf.org/html/rfc8232) Procedures for a Stateful PCE
% * [RFC 8281](https://tools.ietf.org/html/rfc8281) PCE-Initiated LSP Setup
% * [RFC 5541](https://tools.ietf.org/html/rfc5541) Encoding of Objective Functions
% * [RFC 8282](https://tools.ietf.org/html/rfc8282) Inter-Layer MPLS and GMPLS Traffic Engineering
% * [RFC 8408](https://tools.ietf.org/html/rfc8408) Conveying Path Setup Type
% * [RFC 8664](https://tools.ietf.org/html/rfc8664) Extensions for Segment Routing
%     [draft-ietf-pce-segment-routing-07](https://tools.ietf.org/html/draft-ietf-pce-segment-routing-07)
%     [draft-ietf-pce-segment-routing-16](https://tools.ietf.org/html/draft-ietf-pce-segment-routing-16)
% * [RFC 7470](https://tools.ietf.org/html/rfc7470) Conveying Vendor-Specific Constraints
% * [RFC 8697](https://tools.ietf.org/html/rfc8697) Extensions for Establishing Relationships between Sets of LSPs
% * [draft-ietf-pce-segment-routing-policy-cp-01](https://tools.ietf.org/html/draft-ietf-pce-segment-routing-policy-cp-01) Extension to support Segment Routing Policy Candidate Paths

% TODOS:
%   - Implemente SVEC support
%       https://tools.ietf.org/html/rfc6007
%   - Implemente RSVP Error Spec TLV
%       https://tools.ietf.org/html/rfc8231#section-7.3.4
%   - Implemente Objective Functions
%       https://tools.ietf.org/html/rfc5541
%   - Implemenve PCE-Initiated LSP Seyup
%       https://tools.ietf.org/html/rfc8281
%   - Implemente Vendor-Specific Constraints
%       https://tools.ietf.org/html/rfc7470
%   - Implemente Relationships between Sets of LSPs
%       https://tools.ietf.org/html/rfc8697
%   - Implemente Candidate Path
%       https://tools.ietf.org/html/draft-ietf-pce-segment-routing-policy-cp-01

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("stdlib/include/assert.hrl").

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([decode/1]).
-export([encode/1]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(IoList) when is_list(IoList) ->
    decode(list_to_binary(IoList));
decode(Bin) when is_binary(Bin) ->
    case pcep_codec_message:decode(pcep_codec_context:new(), Bin) of
        {more, Length, _Ctx} ->
            {more, Length};
        {error, Reason, RemData, Ctx} ->
            Error = pcep_codec_context:get_error(Ctx),
            Warnings = pcep_codec_context:get_warnings(Ctx),
            {error, Reason, Error, Warnings, RemData};
        {ok, Message, RemData, Ctx} ->
            ?assert(undefined =:= pcep_codec_context:get_error(Ctx)),
            Warnings = pcep_codec_context:get_warnings(Ctx),
            {ok, Message, Warnings, RemData}
    end.

encode(Obj) when is_tuple(Obj) ->
    case pcep_codec_message:encode(pcep_codec_context:new(), Obj) of
        {error, Reason, Ctx} ->
            Error = pcep_codec_context:get_error(Ctx),
            Warnings = pcep_codec_context:get_warnings(Ctx),
            {error, Reason, Error, Warnings};
        {ok, Data, Ctx} ->
            ?assert(undefined =:= pcep_codec_context:get_error(Ctx)),
            Warnings = pcep_codec_context:get_warnings(Ctx),
            {ok, Data, Warnings}
    end.
