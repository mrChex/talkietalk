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

init([{Module, Fun}, Chat]) ->
  #{
    id := ChatId,
    type := Type
  } = Chat,

  StateName = {Module, Fun},
  Debug = application:get_env(talkietalk, debug, false),
  State = #{
    id => ChatId,
    type => Type,
    debug => Debug
  },

  {ok, StateName, State}.


handle_event({msg, #{text := <<"/terminate">>}}, _, #{debug := Debug} = State) when Debug =:= true->
  {stop, normal, State};

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

handle_event({callback, From, Chat, MessageId, Q}, StateFullName, State)->
  {CallbackModule, CallbackFun} = application:get_env(talkietalk, callback_handler, {talkietalk_example_callback, handle}),

  {Answer, NewStateName, NewState} = case CallbackModule:CallbackFun({From, Chat, MessageId, Q}, State) of
    {answer, CallbackAnswer, CallbackState} -> {CallbackAnswer, StateFullName, CallbackState};
    {next_state, NextStateFullName, NextState} -> {#{}, NextStateFullName, NextState};
    noanswer -> {#{}, StateFullName, State}
  end,

  talkietalk_telegram:answerCallbackQuery(Answer#{
    callback_query_id => maps:get(query_id, Q)
  }),

  {next_state, NewStateName, NewState};

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
