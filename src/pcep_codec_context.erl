-module(pcep_codec_context).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("stdlib/include/assert.hrl").

-include("pcep_codec.hrl").
-include("pcep_codec_internal.hrl").


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([new/0]).
-export([get/2]).
-export([begin_message/2]).
-export([ignore_object/1]).
-export([begin_object/3]).
-export([commit_object/1]).
-export([begin_subobj/2]).
-export([ignore_tlv/1]).
-export([begin_tlv/2]).
-export([ignore_sub_tlv/1]).
-export([begin_sub_tlv/2]).
-export([get_path/1]).
-export([reset_error/1]).
-export([set_error/2]).
-export([update_error/2]).
-export([get_error/1]).
-export([add_warning/2]).
-export([get_warnings/1]).



%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() -> #{}.

begin_message(Ctx, Type) ->
    Ctx#{message_type => Type,
         object_count => 0,
         object_index => 0,
         object_class => undefined,
         object_type => undefined,
         object_data => undefined,
         tlv_index => 0,
         tlv_type => undefined,
         tlv_data => undefined,
         sub_tlv_index => 0,
         sub_tlv_type => undefined,
         sub_tlv_data => undefined,
         subobj_index => 0,
         subobj_type => undefined,
         subobj_data => undefined
    }.

ignore_object(#{object_count := Idx} = Ctx) ->
    Ctx#{object_index => Idx + 1,
         object_class => undefined,
         object_type => undefined,
         object_data => undefined,
         tlv_type => undefined,
         tlv_index => 0,
         tlv_data => undefined,
         sub_tlv_type => undefined,
         sub_tlv_index => 0,
         sub_tlv_data => undefined,
         subobj_index => 0,
         subobj_type => undefined,
         subobj_data => undefined
    }.

begin_object(#{object_count := Idx} = Ctx, Class, Type) ->
    Ctx#{object_index => Idx + 1,
         object_class => Class,
         object_type => Type,
         tlv_index => 0,
         tlv_type => undefined,
         tlv_data => undefined,
         sub_tlv_index => 0,
         sub_tlv_type => undefined,
         sub_tlv_data => undefined,
         subobj_index => 0,
         subobj_type => undefined,
         subobj_data => undefined
    };
begin_object(Ctx, Class, Type) ->
    begin_object(Ctx#{object_count => 0}, Class, Type).

commit_object(#{object_count := Count} = Ctx) ->
    Ctx#{object_count => Count + 1}.

begin_subobj(#{subobj_index := Idx} = Ctx, Type) ->
    Ctx#{subobj_index => Idx + 1,
         subobj_type => Type
    }.

ignore_tlv(#{tlv_index := Idx} = Ctx) ->
    Ctx#{tlv_index => Idx + 1,
         sub_tlv_index => 0,
         sub_tlv_type => undefined,
         sub_tlv_data => undefined
    }.

begin_tlv(#{tlv_index := Idx} = Ctx, Type) ->
    Ctx#{tlv_index => Idx + 1,
         tlv_type => Type,
         sub_tlv_index => 0,
         sub_tlv_type => undefined,
         sub_tlv_data => undefined
    }.

ignore_sub_tlv(#{sub_tlv_index := Idx} = Ctx) ->
    Ctx#{sub_tlv_index => Idx + 1}.

begin_sub_tlv(#{sub_tlv_index := Idx} = Ctx, Type) ->
    Ctx#{sub_tlv_index => Idx + 1,
         sub_tlv_type => Type
    }.

get(Ctx, Name) ->
    maps:get(Name, Ctx, undefined).

get_path(#{message_type := MsgType,
           object_index := ObjIdx, object_type := ObjType,
           tlv_index := TlvIdx, tlv_type := TlvType,
           sub_tlv_index := SubTlvIdx, sub_tlv_type := SubTlvType,
           subobj_index := SubIdx, subobj_type := SubType}) ->
    make_path([{msg, MsgType},
               {obj, {ObjIdx, ObjType}},
               {tlv, {TlvIdx, TlvType}},
               {'sub-tlv', {SubTlvIdx, SubTlvType}},
               {'sub-obj', {SubIdx, SubType}}]);
get_path(_Ctx) ->
    undefined.

reset_error(Ctx) ->
    maps:remove(error, Ctx).

set_error(Ctx, Error) ->
    ?assert(not maps:is_key(error, Ctx)),
    Ctx#{error => Error}.

update_error(#{error := CurrError} = Ctx, NewError) ->
    Ctx#{error => maps:merge(NewError, CurrError)}.

get_error(#{error := Error}) -> Error;
get_error(_Ctx) -> undefined.

add_warning(#{warnings := Warnings} = Ctx, Warn) ->
    Ctx#{warnings => Warnings ++ [Warn]};
add_warning(Ctx, Warn) ->
    Ctx#{warnings => [Warn]}.

get_warnings(#{warnings := Warnings}) -> Warnings;
get_warnings(_Ctx) -> [].


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_path(Parts) -> make_path(Parts, []).

make_path([], Acc) -> lists:flatten(lists:reverse(Acc));
make_path([{_Tag, undefined} | Rem], Acc) -> make_path(Rem, Acc);
make_path([{_Tag, {_, undefined}} | Rem], Acc) -> make_path(Rem, Acc);
make_path([{Tag, {Idx, Name}} | Rem], Acc) ->
    make_path(Rem, [io_lib:format("/~s[~w]:~s", [Tag, Idx, Name]) | Acc]);
make_path([{Tag, Name} | Rem], Acc) ->
    make_path(Rem, [io_lib:format("/~s:~s", [Tag, Name]) | Acc]).
