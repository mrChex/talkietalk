-module(talkietalk_example_welcome).

%% API
-export([
  main/3,
  crash_it/3
]).


main(#{text := <<"/simpleKeyboard">>}, ChatId, State)->
  talkietalk_telegram:sendMessage(ChatId, <<"Хочешь крешнусь?"/utf8>>, null, #{
    keyboard => [
      [<<"Да"/utf8>>, <<"Нет"/utf8>>]
    ]
  }),
  {next_state, {?MODULE, crash_it}, State};

main(#{text := <<"/inline">>}, ChatId, State) ->

  Response = talkietalk_telegram:sendMessage(ChatId, <<"Нажми одну из кнопок"/utf8>>, null, #{
    inline_keyboard => [
      [
        #{text => <<"Тестик 1"/utf8>>,
          callback_data => <<"test1">>},
        #{text => <<"Тестик 2"/utf8>>,
          callback_data => <<"test2">>
        }
      ]
    ]
  }),

  % be avare of using State as datastorage like this :)
  % this is only example
  #{<<"message_id">> := MessageId} = Response,
  MsgIds = maps:get(msg_ids, State, #{}),

  {next_state, {?MODULE, main}, State#{
    msg_ids => MsgIds#{ MessageId => [] }
  }};

main(_, ChatId, State)->
  Msg = <<"Ку. Что тестируем?\n"/utf8,
    "/inline Inline keyboards and on-the-fly updating\n",
    "/simpleKeyboard Simple keyboard"
  >>,

  talkietalk_telegram:sendMessage(ChatId, Msg),
  {next_state, {?MODULE, main}, State}.


crash_it(#{text := <<"Да"/utf8>>}, _ChatId, _State)->
  io:format("~p", test),
  ok;

crash_it(#{text := <<"Нет"/utf8>>}, ChatId, State)->
  talkietalk_telegram:sendMessage(ChatId, <<"Ладно"/utf8>>, null, #{hide_keyboard => true}),
  {next_state, {?MODULE, main}, State};
crash_it(_,_,_) -> unknown.