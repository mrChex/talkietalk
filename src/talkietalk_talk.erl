-module(talkietalk_talk).

-behaviour(gen_fsm).

%% API
-export([start/2]).

%% gen_fsm callbacks
-export([
  init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
]).

start(TalkieModule, Msg) -> gen_fsm:start(?MODULE, [TalkieModule, Msg], []).

init([{Module, Fun}, Msg]) ->
  #{chat := #{
    id := ChatId,
    type := Type
  }} = Msg,

  StateName = {Module, Fun},

  {ok, StateName, #{
    id => ChatId,
    type => Type
  }}.


handle_event({msg, Msg}, {StateModule, StateName} = StateFullName, #{id := ChatId} = State)->

  case StateModule:StateName(Msg, ChatId, State) of
    unknown ->
      #{text := _Text, from := #{first_name := FirstName}} = Msg,
      StateNameBin = atom_to_binary(StateName, utf8),
      StateModuleBin = atom_to_binary(StateModule, utf8),
      talkietalk_telegram:sendMessage(ChatId,
        <<FirstName/binary, ", я не знаю как на это отвечать в состоянии "/utf8,
          StateModuleBin/binary, ":"/utf8, StateNameBin/binary>>
      ),
      {next_state, StateFullName, State};
    Response -> Response
  end;

handle_event({msg, Msg}, StateName, #{id := ChatId} = State)->
  case talk:StateName(Msg, ChatId, State) of
    unknown ->
      #{text := _Text, from := #{first_name := FirstName}} = Msg,
      StateNameBin = atom_to_binary(StateName, utf8),
      talkietalk_telegram:sendMessage(ChatId, <<FirstName/binary, ", я не знаю как на это отвечать в состоянии "/utf8, StateNameBin/binary>>),
      {next_state, StateName, State};
    Response -> Response
  end;

handle_event(Event, StateName, State) ->
  io:format("SOME EVENT IN TALK! ~p~n", [Event]),
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
