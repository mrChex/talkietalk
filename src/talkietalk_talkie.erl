-module(talkietalk_talkie).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

% api
-export([
  talks/0,
  message/1,
  callback_query/1,
  inline_query/1
]).


-define(SERVER, ?MODULE).

% https://core.telegram.org/bots/api#chat
-type chat() :: #{
  id => integer(),
  first_name => binary(),
  type => private | group | supergroup | channel,
  % optional fields
  title => binary() | undefined,
  first_name => binary(),
  last_name => binary() | undefined,
  username => binary() | undefined
}.

% https://core.telegram.org/bots/api#user
-type user() :: #{
  id => integer(),
  first_name => binary(),
  % optional fields
  last_name => binary() | undefined,
  username => binary() | undefined
}.

-type msg() :: #{
  chat => chat(),
  date => integer(),
  from => user(),
  msgId => integer(),
  text => binary() | undefined,
  contact => user() | undefined
}.

% Talk instance
-type talk() :: #{
  chat => chat(),
  last_ping => integer(),
  pid => pid()
}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TalkieModule) ->

  gen_server:start_link({local, ?SERVER}, ?MODULE, [TalkieModule], []).

talks() -> gen_server:call(?SERVER, talks).

message(Msg) when is_map(Msg)->
  #{
    <<"chat">> := Chat,
    <<"date">> := Date,
    <<"from">> := From,
    <<"message_id">> := MsgId
  } = Msg,

  Text = maps:get(<<"text">>, Msg, undefined),
  Contact = maps:get(<<"contact">>, Msg, undefined),

  message(chatToType(Chat), Date, userToType(From), MsgId, Text, Contact).

% https://core.telegram.org/bots/api#callbackquery
callback_query(Query) when is_map(Query) ->
  % required fields
  #{
    <<"id">> := QueryId,  % Unique identifier for this query
    <<"from">> := From,  % Sender
    <<"chat_instance">> := ChatInstance  % Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
  } = Query,

  % optionals

  % Optional. Message with the callback button that originated the query. Note that message content and
  % message date will not be available if the message is too old
  Message = maps:get(<<"message">>, Query, null),

  % Optional. Identifier of the message sent via the bot in inline mode, that originated the query.
  InlineMessageId = maps:get(<<"inline_message_id">>, Query, null),

  % Optional. Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
  Data = maps:get(<<"data">>, Query, null),

  % Optional. Short name of a Game to be returned, serves as the unique identifier for the game
  GameShortName = maps:get(<<"game_short_name">>, Query, null),

  callback(QueryId, From, ChatInstance, Message, InlineMessageId, Data, GameShortName).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([TalkieModule]) ->
  {ok, #{
    talks => [], % list of talks
    talkie_module => TalkieModule
  }}.

handle_call(talks, _, #{talks := Talks} = State)->
  {reply, Talks, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({chat_msg, #{chat := Chat} = Msg}, State) ->

  {TalkPid, NewState} = get_or_create_talk(Chat, State),
  send_to_talk(TalkPid, {msg, Msg}),

  {noreply, NewState};

handle_cast({callback, #{ message := null } = Q}, State)->
  io:format("Cant handle callback. Message is too old. Query: ~p~n", [Q]),
  {noreply, State};
handle_cast({callback, #{
  query_id := _,
  from := From,
  chat_instance := _,
  message := Message,
  inline_message_id := _,
  data := _,
  game_short_name := _
} = Q}, State)->
  #{
    <<"chat">> := Chat0,
    <<"message_id">> := MessageId
  } = Message,
  Chat = chatToType(Chat0),

  {TalkId, NewState} = get_or_create_talk(Chat, State),
  send_to_talk(TalkId, {callback, userToType(From), Chat, MessageId, Q}),

  {noreply, NewState};

handle_cast({inline_query, #{from := From} = Query}, State)->
  {TalkId, NewState} = get_or_create_talk(From, State),
  send_to_talk(TalkId, {inline_query, Query}),
  {noreply, NewState};


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _, process, TalkPid, Reason}, #{talks := Talks} = State)->
  case popTalk(#{pid => TalkPid}, Talks) of
    {no_talk, _} ->
      io:format("Wow! Down talk, but talk never exist. Why? Maybe bug~n"),
      {noreply, State};
    {Talk, NewTalks} ->
      #{chat_id := ChatId} = Talk,
      talkietalk_telegram:sendMessage(ChatId, <<"😵"/utf8>>),  % here emoji in binary X-(
      {noreply, State#{talks => NewTalks}}
  end;

handle_info(Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to_talk(TalkPid, Msg)->
  gen_fsm:send_all_state_event(TalkPid, Msg).

get_or_create_talk(#{id := ChatId} = Chat, #{talks := Talks, talkie_module := TalkieModule} = State) ->
  case getTalk(ChatId, Talks) of
    no_talk ->
      {ok, TalkPid} = talkietalk_talk:start(TalkieModule, Chat),
      erlang:monitor(process, TalkPid),
      Talk = #{
        chat_id => ChatId,
        pid => TalkPid
      },

      {TalkPid, State#{talks => [Talk|Talks]}};

    #{pid := TalkPid} = Talk ->
      {TalkPid, State#{talks => pingTalk(Talk, Talks)}}
  end.

chatToType(#{<<"id">> := ChatId, <<"type">> := TypeBin} = Chat) ->
  Type = case TypeBin of
    <<"private">> -> private;
    <<"group">> -> group;
    <<"supergroup">> -> supergroup;
    <<"channel">> -> channel
  end,

  #{
    id => ChatId,
    type => Type,
    first_name => maps:get(<<"first_name">>, Chat, undefined),
    last_name => maps:get(<<"last_name">>, Chat, undefined),
    username => maps:get(<<"username">>, Chat, undefined)
  }.

userToType(#{<<"id">> := UserId, <<"first_name">> := FirstName} = User)->
  #{
    id => UserId,
    first_name => FirstName,
    last_name => maps:get(<<"last_name">>, User, undefined),
    username => maps:get(<<"username">>, User, undefined)
  }.


-spec message(chat(), integer(), user(), integer(), binary() | undefined, user() | undefined)-> ok.
message(Chat, Date, From, MsgId, Text, Contact)->
  gen_server:cast(?SERVER, {chat_msg, #{
    chat => Chat,
    date => Date,
    from => From,
    msgId => MsgId,
    text => Text,
    contact => Contact
  }}).

callback(QueryId, From, ChatInstance, Message, InlineMessageId, Data, GameShortName) ->
  gen_server:cast(?SERVER, {callback, #{
    query_id => QueryId,
    from => From,
    chat_instance => ChatInstance,
    message => Message,
    inline_message_id => InlineMessageId,
    data => Data,
    game_short_name => GameShortName
  }}).

inline_query(#{<<"id">> := Id, <<"from">> := From, <<"query">> := Query, <<"offset">> := Offset} = Q) ->
  Location = maps:get(<<"location">>, Q, null),
  gen_server:cast(?SERVER, {inline_query, #{
    id => Id,
    from => userToType(From),
    query => Query,
    offset => Offset,
    location => Location
  }}).

-spec getTalk(binary(), [talk()]) -> talk() | no_talk.
getTalk(_, []) -> no_talk;
getTalk(ChatId, [#{chat_id := ChatId} = Talk|_])-> Talk;
getTalk(ChatId, [_|Talks]) -> getTalk(ChatId, Talks).

-spec pingTalk(talk(), [talk()])-> [talk()].
pingTalk(Talk, Talks)-> pingTalk(Talk, Talks, []).

pingTalk(_, [], Acc)-> Acc;
pingTalk(#{pid := Pid}, [#{pid := Pid} = Talk|Tail], Acc)->
  [Talk#{last_ping => get_timestamp()}|Tail] ++ Acc;
pingTalk(Talk, [Head|Talks], Acc) -> pingTalk(Talk, Talks, [Head|Acc]).


popTalk(Talk, Talks)-> popTalk(Talk, Talks, []).
popTalk(_, [], Acc)-> {no_talk, Acc};
popTalk(#{pid := Pid}, [#{pid := Pid} = Talk|Tail], Acc)->
  {Talk, Tail ++ Acc};
popTalk(Talk, [Head|Talks], Acc) -> pingTalk(Talk, Talks, [Head|Acc]).



get_timestamp() ->
  {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
  MegaSeconds * 1000000 * 1000000 + Seconds * 1000000 + MicroSeconds. %% Microseconds