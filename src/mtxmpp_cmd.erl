%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>

-module(mtxmpp_cmd).

-behaviour(gen_server).

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("mtxmpp.hrl").

-export([start_link/1]).

-export([
         spark/2,
         available/2,
         unavailable/2
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
-define(CMD_TIMEOUT, 60000).

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

-spec available(pid(), #received_packet{}) -> ok.
available(_Session, #received_packet{from = _From} = _Packet) ->
  %% FromJID = exmpp_jid:make(From),
  %% ?DBG("Check data availability to send to JID ~p", [FromJID]),
  %% TODO
  ok.

-spec unavailable(pid(), #received_packet{}) -> ok.
unavailable(Session, #received_packet{from = From} = Packet) ->
  ?DBG("Stop worker", []),
  FromJID = exmpp_jid:make(From),
  Pid = gproc:lookup_local_name(FromJID),
  if is_pid(Pid) ->
      gen_server:cast(Pid, {stop, Session, Packet});
     true ->
      ok
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
  {noreply, State, ?CMD_TIMEOUT};
handle_cast({stop, _Session, #received_packet{} = _Packet},
            #state{jid = JID} = State) ->
  ?DBG("JID ~p unavailable", [JID]),
  {stop, normal, State};
handle_cast(_Msg, State) ->
  ?DBG("Unhandled cast:~n~p", [_Msg]),
  {noreply, State}.

handle_info(timeout, #state{jid = JID} = State) ->
  ?DBG("Stop CMD for ~p on timeout", [JID]),
  {stop, normal, State};
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
      Match = re:run(CData, "#([[:digit:]]+)(/([[:digit:]]+)?)?\s*(.*)\s*", [{capture, [1,3,4], binary}, dotall, unicode]),
      ?DBG("Match:~n~p", [Match]),
      case Match of
        {match, [PostId, <<>> = CommentId, <<>>]} ->
          cmd_show(PostId, CommentId);
        {match, [PostId, CommentId, <<>>]} ->
          cmd_show(PostId, CommentId);
        {match, [PostId, <<>> = CommentId, CommentReply]} ->
          cmd_reply(PostId, CommentId, CommentReply);
        {match, [PostId, CommentId, CommentReply]} ->
          cmd_reply(PostId, CommentId, CommentReply);
        _Other ->
          ?DBG("Bad Match:~n~p", [_Other]),
          [#xmlcdata{cdata = <<"Bad command">>}]
      end;
    [#xmlcdata{cdata = <<BCT, _/binary>> = BlogsCircsTags} | PostData]
      when BCT =:= $% orelse
           BCT =:= $$ orelse
           BCT =:= $* ->
      {Blogs, Circles, Tags} =
        lists:foldl(fun(<<$%, Blog/binary>>, {B, C, T}) ->
                        {B++[Blog], C, T};
                       (<<$$, Circle/binary>>, {B, C, T}) ->
                        {B, C++[Circle], T};
                       (<<$*, Tag/binary>>, {B, C, T}) ->
                        {B, C, T++[Tag]}
                    end, {[], [], []},
                    re:split(BlogsCircsTags, "[[:space:]]+", [unicode])),
      ?DBG("Blogs: ~p~nCircles: ~p~nTags: ~p", [Blogs, Circles, Tags]),
      cmd_post(Blogs, Circles, Tags, PostData);
    PostData ->
      cmd_post([], [], [], PostData)
  end.

cmd_show(PostId, <<>>) ->
  case mtc_entry:sget(mt_post, PostId) of
    #mt_post{body = Body,
             author = Author,
             comments = Comments} ->
      ComLen = length(Comments),
      ?DBG("Post ~p:~n~p", [PostId, Body]),
      [
       #xmlcdata{cdata = <<"@", (Author#mt_person.name)/binary>>},
       #xmlcdata{cdata = <<"\n">>},
       #xmlcdata{cdata = Body},
       #xmlcdata{cdata = <<"\n">>},
       #xmlcdata{cdata = iolist_to_binary(["#", PostId, if (ComLen>0) -> [" (", integer_to_list(ComLen), " replies)"]; true -> [] end])}
      ];
    undefined ->
      [#xmlcdata{cdata = iolist_to_binary(["Post #", PostId, " not found"])}]
  end;
cmd_show(PostId, CommentId) ->
  case mtc_entry:sget(mt_post, PostId) of
    #mt_post{body = _Body,
             author = _Author,
             comments = Comments} ->
      CID = list_to_integer(binary_to_list(CommentId)),
      case [Comment || #mt_comment{id = Id} = Comment <- Comments, Id =:= CID] of
        [#mt_comment{author = Author, body = CommentBody}] ->
          ?DBG("Comment #~p/~p:~n~p", [PostId, CommentId, CommentBody]),
          [
           #xmlcdata{cdata = <<"@", (Author#mt_person.name)/binary>>},
           #xmlcdata{cdata = <<"\n">>},
           #xmlcdata{cdata = CommentBody},
           #xmlcdata{cdata = <<"\n">>},
           #xmlcdata{cdata = iolist_to_binary(["#", PostId, "/", CommentId])}
          ];
        [] ->
          [#xmlcdata{cdata = iolist_to_binary(["Comment #", PostId, "/", CommentId, " not found"])}]
      end;
    undefined ->
      [#xmlcdata{cdata = iolist_to_binary(["Post #", PostId, " not found"])}]
  end.

cmd_reply(PostId, <<>>, Reply) ->
  Author = #mt_person{
    id = 1,
    name = <<"Zert">>
   },
  Comment = #mt_comment{
    post_id = PostId,
    parents = [PostId],
    author = Author,
    body = Reply,
    origin = ?MT_ORIGIN
   },
  CommentId = mtc_entry:sput(Comment),
  [#xmlcdata{cdata = <<"Post reply #", PostId/binary, "/", CommentId/binary>>}];
cmd_reply(PostId, CommentId, _ReplyData) ->
  [#xmlcdata{cdata = <<"Comment reply #", PostId/binary, "/", CommentId/binary>>}].


cmd_post(Blogs, Circles, Tags, PostData) ->
  Author = #mt_person{
    id = 1,
    name = <<"Zert">>
   },
  Post = #mt_post{
    author = Author,
    body = iolist_to_binary([CData || #xmlcdata{cdata = CData} <- PostData]),
    origin = ?MT_ORIGIN
   },
  NewPostId = mtc_entry:sput(Post),
  [
   #xmlcdata{cdata = <<"\n">>},
   #xmlcdata{cdata = <<"New message posted: #", NewPostId/binary>>},
   #xmlcdata{cdata = <<"\n">>},
   #xmlcdata{cdata = if (length(Blogs) > 0) -> iolist_to_binary([<<"% ">>, [[" ", B] || B <- Blogs], "\n"]); true -> <<>> end},
   #xmlcdata{cdata = if (length(Circles) > 0) -> iolist_to_binary([<<"$ ">>, [[" ", C] || C <- Circles], "\n"]); true -> <<>> end},
   #xmlcdata{cdata = if (length(Tags) > 0) -> iolist_to_binary([<<"* ">>, [[" ", T] || T <- Tags], "\n"]); true -> <<>> end}
   | PostData
  ].

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
