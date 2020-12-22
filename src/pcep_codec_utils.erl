-module(pcep_codec_utils).

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([bit2bool/1]).
-export([bool2bit/1]).
-export([addr4/1, addr6/1]).
-export([padding/1]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bit2bool(1) -> true;
bit2bool(0) -> false.

bool2bit(true) -> 1;
bool2bit(false) -> 0.

addr4({A, B, C, D}) -> 
    <<A:8, B:8, C:8, D:8>>.

addr6({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>.

padding(Size) -> <<0:(8 * (-Size band 3))>>.
