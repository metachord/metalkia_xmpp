%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


-module(mtxmpp).

-export([
  get_env/1,
  get_env/2
]).

get_env(Param) ->
  mtc_util:get_env(?MODULE, Param, undefined).

get_env(Param, Default) ->
  mtc_util:get_env(?MODULE, Param, Default).
