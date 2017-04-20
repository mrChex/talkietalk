-module(talkietalk_example_welcome).

%% API
-export([
  main/3,
  how_are_you/3
]).

main(Msg, ChatId, State)->
  io:format("Yo!(~p) New msg: ~p~n", [ChatId, Msg]),
  talkietalk_telegram:sendMessage(ChatId, <<"Воу воу! Перехожу в новый стейт. Как дела?"/utf8>>),
  {next_state, {?MODULE, how_are_you}, State}.

how_are_you(Msg, ChatId, State)->
  talkietalk_telegram:sendMessage(ChatId, <<"пффф"/utf8>>),
  {next_state, {?MODULE, main}, State}.