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
  message/1
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


handle_cast({chat_msg, #{chat := Chat} = Msg}, #{talks := Talks, talkie_module := TalkieModule} = State) ->

  case getTalk(Chat, Talks) of
    no_talk ->
      {ok, TalkPid} = talkietalk_talk:start(TalkieModule, Msg),
      erlang:monitor(process, TalkPid),
      Talk = #{
        chat => Chat,
        pid => TalkPid
      },
      gen_fsm:send_all_state_event(TalkPid, {msg, Msg}),
      {noreply, State#{talks => [Talk|Talks]}};

    #{pid := TalkPid} = Talk ->
      gen_fsm:send_all_state_event(TalkPid, {msg, Msg}),
      {noreply, State#{talks => pingTalk(Talk, Talks)}}
  end;


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _, process, TalkPid, Reason}, #{talks := Talks} = State)->
  case popTalk(#{pid => TalkPid}, Talks) of
    {no_talk, _} ->
      io:format("Wow! Down talk, but talk never exist. Why? Maybe bug~n"),
      {noreply, State};
    {Talk, NewTalks} ->
      #{chat := #{id := ChatId}} = Talk,
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

-spec getTalk(chat(), [talk()]) -> talk() | no_talk.
getTalk(_, []) -> no_talk;
getTalk(#{id := ChatId}, [#{chat := #{id := ChatId}} = Talk|_])-> Talk;
getTalk(Chat, [_|Talks]) -> getTalk(Chat, Talks).

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