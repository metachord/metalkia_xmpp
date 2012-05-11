%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>

-module(mtxmpp_sup).

-include_lib("metalkia_core/include/mt_log.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([
         start_cmd/1
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_cmd(Args) ->
  supervisor:start_child(mtxmpp_cmd_sup, [Args]).

init([mtxmpp_cmd = ModName]) ->
  ?DBG("Command Worker supervisor start", []),
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  AChild =
    [
     {ModName, {ModName, start_link, []},
      Restart, Shutdown, Type, [ModName]}
    ],
  {ok, {SupFlags, AChild}};

init([]) ->
  ?DBG("Start Metalk XMPP sup", []),
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  ChildSup =
    [
     {mtxmpp_cmd_sup,
      {supervisor, start_link,
       [{local, mtxmpp_cmd_sup}, ?MODULE, [mtxmpp_cmd]]},
      Restart, Shutdown, supervisor, [?MODULE]}
    ],

  IsEnabled = mtxmpp:get_env(enabled, false),
  Children =
    if IsEnabled ->
        [
         {mtxmpp_client, {mtxmpp_client, start_link, []},
          Restart, Shutdown, Type, [mtxmpp_client]} |
         ChildSup
        ];
       true ->
        ChildSup
    end,

  {ok, {SupFlags, Children}}.

%%% Internal functions
