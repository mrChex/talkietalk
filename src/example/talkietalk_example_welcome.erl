-module(talkietalk_example_welcome).

%% API
-export([
  main/3,
  how_are_you/3,
  crash_it/3
]).

main(Msg, ChatId, State)->
  io:format("Yo!(~p) New msg: ~p~n", [ChatId, Msg]),
  talkietalk_telegram:sendMessage(ChatId, <<"Воу воу! Перехожу в новый стейт. Как дела?"/utf8>>),
  {next_state, {?MODULE, how_are_you}, State}.



how_are_you(_Msg, ChatId, State)->
  talkietalk_telegram:sendMessage(ChatId, <<"Хочешь крешнусь?"/utf8>>, null, #{
    keyboard => [
      [<<"Yes">>, <<"No">>]
    ]
  }),
  {next_state, {?MODULE, crash_it}, State}.

crash_it(#{text := <<"Yes">>}, _ChatId, _State)->
  io:format("~p", test),
  ok;

crash_it(#{text := <<"No">>}, ChatId, State)->
  talkietalk_telegram:sendMessage(ChatId, <<"Ладно"/utf8>>, null, #{hide_keyboard => true}),
  {next_state, {?MODULE, main}, State};
crash_it(_,_,_) -> unknown.