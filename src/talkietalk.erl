-module(talkietalk).

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

start(_, _) ->
  TalkModule = application:get_env(talkietalk, initial_talk, {talkietalk_example_welcome, main}),
  case application:get_env(talkietalk, telegram_token, undefined) of
    undefined -> {stop, normal, <<"You should specify telegram token in application config">>};
    TelegramToken ->
      talkietalk_sup:start_link(TalkModule, TelegramToken)
  end.

stop(_State) ->
  ok.
