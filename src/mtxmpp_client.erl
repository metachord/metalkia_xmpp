%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtxmpp_client).

-behaviour(gen_server).

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).
-define(MT_DEFAULT_RESOURCE, <<"main">>).
-record(state, {
          session,
          jid_str,
          jid,
          jid_login,
          password
         }).

%%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
  JID = mtxmpp_app:get_env(jid, undefined),
  Password = mtxmpp_app:get_env(password, undefined),

  Session = exmpp_session:start(),
  [User, Server] = string:tokens(JID, "@"),
  EJID = exmpp_jid:make(User, Server, ?MT_DEFAULT_RESOURCE),
  exmpp_session:auth_basic_digest(Session, EJID, Password),
  {ok, _StreamId} = exmpp_session:connect_TCP(Session, Server, 5222),
  gen_server:cast(self(), login),
  {ok, #state{
     session = Session,
     jid_str = JID,
     jid = EJID,
     password = Password
    }}.

handle_call(Request, _From, State) ->
  ?DBG("Unhandled call: ~p", [Request]),
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast(login, #state{
              jid_str = JIDs,
              session = Session
             } = State) ->
  ?DBG("Try login with ~p", [JIDs]),
  NewState =
  try exmpp_session:login(Session) of
      {ok, JIDLogin} ->
      ?DBG("JID login: ~p", [JIDLogin]),
      exmpp_session:send_packet(
        Session,
        exmpp_presence:set_status(
          exmpp_presence:available(), "Metalkia Ready")),
      State#state{jid_login = JIDLogin}
  catch
    throw:{auth_error, 'not-authorized'} ->
      gen_server:cast(self(), register),
      State
  end,
  {noreply, NewState};
handle_cast(register, #state{
              jid_str = JIDs,
              session = Session,
              password = Password
             } = State) ->
  ?DBG("Register account ~p", [JIDs]),
  exmpp_session:register_account(Session, Password),
  gen_server:cast(self(), login),
  {noreply, State};
handle_cast(_Msg, State) ->
  ?DBG("Unhandled cast: ~p", [_Msg]),
  {noreply, State}.

handle_info(#received_packet{
               packet_type = presence,
               type_attr = "available",
               from = {Fnode, Fdomain, Fresource}
              },
            #state{jid = #jid{
                     node = Fnode,
                     domain = Fdomain,
                     resource = Fresource
                    } = _EJID} = State) ->
  ?DBG("Me is ready", []),
  {noreply, State};

handle_info(#received_packet{
               packet_type = presence,
               type_attr = TypeAttr,
               from = From
              } = RPacket,
            #state{
                    session = Session
                  } = State) ->
  FromJID = exmpp_jid:make(From),
  case TypeAttr of
    "subscribe" ->
      presence_subscribed(Session, FromJID),
      presence_subscribe(Session, FromJID);
		"subscribed" ->
      presence_subscribed(Session, FromJID),
      presence_subscribe(Session, FromJID);
    "available" ->
      %% ?DBG("Available ~p", [FromJID]),
      mtxmpp_cmd:available(Session, RPacket);
    "unavailable" ->
      ?DBG("Unavailable ~p", [FromJID]),
      mtxmpp_cmd:unavailable(Session, RPacket);
    _Other ->
      ?DBG("Unhandled TypeAttr ~p", [_Other]),
      ok
  end,
  {noreply, State};

handle_info(#received_packet{
               packet_type = iq,
               type_attr = TypeAttr,
               from = From
              },
            #state{
                    session = _Session
                  } = State) ->
  _FromJID = exmpp_jid:make(From),
  case TypeAttr of
    "set" ->
      ?DBG("IQ ~p", [TypeAttr]),
      ok;
    _Other ->
      ?DBG("Unhandled TypeAttr ~p", [_Other]),
      ok
  end,
  {noreply, State};

handle_info(#received_packet{
               packet_type = message,
               type_attr = TypeAttr
              } = RPacket,
            #state{
                    session = Session
                  } = State) ->
  ?DBG("IQ ~p", [TypeAttr]),
  case TypeAttr of
    "chat" ->
      mtxmpp_cmd:spark(Session, RPacket),
      ok;
    "normal" ->
      %%?DBG("Packet:~n~p", [RPacket]),
      ok;
    _Other ->
      ?DBG("Unhandled TypeAttr ~p", [_Other]),
      ok
  end,
  {noreply, State};

handle_info({stream_error, Error} = ErrReason, State) ->
  ?DBG("Stream Error: ~p", [Error]),
  {stop, ErrReason, State};

handle_info(_Info, State) ->
  ?DBG("Unhandled info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions


presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).
