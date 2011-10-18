-module(mtxmpp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([get_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mtxmpp_sup:start_link().

stop(_State) ->
    ok.

get_env(Key, Default) ->
  gproc:get_env(l, metalkia_xmpp, Key, [app_env, {default, Default}]).
