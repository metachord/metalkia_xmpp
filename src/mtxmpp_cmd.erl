%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtxmpp_cmd).

-behaviour(gen_server).

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").


-export([start_link/1]).

-export([
         spark/2
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          jid
         }).

%%% API
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

-spec spark(pid(), #received_packet{}) -> ok.
spark(Session, #received_packet{from = From} = Packet) ->
  ?DBG("Spark worker", []),
  FromJID = exmpp_jid:make(From),
  Pid = gproc:lookup_local_name(FromJID),
  if is_pid(Pid) ->
      gen_server:cast(Pid, {cmd, Session, Packet});
     true ->
      mtxmpp_sup:start_cmd([Session, Packet])
  end.

%%% gen_server callbacks

init([Session, #received_packet{from = From} = Packet] = Args) ->
  ?DBG("Start with args: ~p", [Args]),
  FromJID = exmpp_jid:make(From),
  true = gproc:add_local_name(FromJID),
  gen_server:cast(self(), {cmd, Session, Packet}),
  {ok, #state{jid = FromJID}}.

handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast({cmd, Session, #received_packet{} = Packet},
            #state{jid = JID} = State) ->
  ?DBG("Packet from:~n~p~n~p", [JID, Packet]),
  packet_action(Session, JID, Packet),
  {noreply, State};
handle_cast(_Msg, State) ->
  ?DBG("Unhandled cast:~n~p", [_Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions

-record(mt_cmd, {
          body,
          misc
         }).

packet_action(MySession, JID, #received_packet{raw_packet = Packet}) ->
  From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
  To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
  TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
  TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
  Elements = exmpp_xml:get_child_elements(TmpPacket2),

  Cmds =
    lists:foldl(fun(#xmlel{ns = 'jabber:client',
                           name = body,
                           children = CDatas},
                    #mt_cmd{body = undefined} = Acc) ->
                    Acc#mt_cmd{body = CDatas};
                   (OtherEl, #mt_cmd{misc = Misc } = Acc) ->
                  Acc#mt_cmd{misc = Misc ++ [OtherEl]}
                end, #mt_cmd{body = undefined, misc = []}, Elements),
  ReplyCDatas = do_cmd(JID, From, Cmds),
  Reply = #xmlel{ns = 'jabber:client',
                 name = body,
                 children = ReplyCDatas},
  TmpPacket3 = exmpp_xml:set_children(TmpPacket2, [Reply | Cmds#mt_cmd.misc]),
  NewPacket = exmpp_xml:remove_attribute(TmpPacket3, <<"id">>),
  exmpp_session:send_packet(MySession, NewPacket).

do_cmd(_JID, From, #mt_cmd{body = Body} = Cmds) ->
  ?DBG("Cmds from ~p:~n~p", [From, Cmds]),
  case Body of
    [#xmlcdata{cdata = <<"HELP", _/binary>>}|_] ->
      cmd_help();
    [#xmlcdata{cdata = <<"NICK", Rest/binary>>}|_] ->
      [#xmlcdata{cdata = <<"Set Nickname to: ", Rest/binary>>}];
    [#xmlcdata{cdata = <<"#", _/binary>> = CData}|_] ->
      Match = re:run(CData, "#([[:digit:]]+)(/([[:digit:]]+)?)?\s*(.*)\s*", [{capture, [1,3,4], binary}, dotall]),
      ?DBG("Match:~n~p", [Match]),
      case Match of
        {match, [PostId, <<>>, <<>>]} ->
          [#xmlcdata{cdata = <<"Show post #", PostId/binary>>}];
        {match, [PostId, CommentId, <<>>]} ->
          [#xmlcdata{cdata = <<"Show comment #", PostId/binary, "/", CommentId/binary>>}];
        {match, [PostId, <<>>, _CommentReply]} ->
          [#xmlcdata{cdata = <<"Post reply #", PostId/binary>>}];
        {match, [PostId, CommentId, _CommentReply]} ->
          [#xmlcdata{cdata = <<"Comment reply #", PostId/binary, "/", CommentId/binary>>}];
        _Other ->
          ?DBG("Bad Match:~n~p", [_Other]),
          [#xmlcdata{cdata = <<"Bad command">>}]
      end;
    _ ->
      NewPostId = <<"12345">>,
      [#xmlcdata{cdata = <<"New message posted: #", NewPostId/binary>>}]
  end.


cmd_help() ->
  [#xmlcdata{cdata = iolist_to_binary(R)} ||
    R <-
      [
       "\n",
       "Metalkia XMPP-bot help:",
       "\n",
       "HELP                 — This message",
       "\n",
       "NICK <nick>          — Set a nickname",
       "\n",
       "#12345               — Show post #12345",
       "\n",
       "#12345/14            — Show 14-th comment of post #12345",
       "\n",
       "#12345 <reply>       — Reply on post #12345",
       "\n",
       "#12345/14 <reply>    — Reply on 14-th comment of post #12345"
      ]].

