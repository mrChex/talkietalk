%%%-------------------------------------------------------------------
%%% @author mrchex
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2017 12:54 PM
%%%-------------------------------------------------------------------
-module(talkietalk_example_callback).
-author("mrchex").

%% API
-export([
  handle/2
]).

handle({From, Chat, MessageId, Q}, State)->
  io:format("CALLBACK HANDLER WORK!: ~p~n", [Q]),
  MsgIds = maps:get(msg_ids, State, #{}),
  case maps:get(MessageId, MsgIds, null) of

    null ->
      talkietalk_telegram:request(<<"editMessageText">>, #{
        chat_id => maps:get(id, Chat),
        message_id => MessageId,
        text => <<"Сообщение устарело."/utf8>>
      }),
      noanswer;

    Items when is_list(Items) ->
      Data = maps:get(data, Q),
      NewItems = [Data|Items],
      talkietalk_telegram:request(<<"editMessageText">>, #{
        chat_id => maps:get(id, Chat),
        message_id => MessageId,
        text => lists:foldr(fun(Item, Acc) ->
          <<Acc/binary, "\n", Item/binary>>
        end, <<"Вот что вы нажимали:"/utf8>>, NewItems),
        reply_markup => #{
          inline_keyboard => [
            [
              #{text => <<"Тестик 1"/utf8>>,
                callback_data => <<"test1">>},
              #{text => <<"Тестик 2"/utf8>>,
                callback_data => <<"test2">>
              }
            ]
          ]
        }
      }),

      {answer, #{text => <<"Answer text :)">>}, State#{
        msg_ids => MsgIds#{
          MessageId => NewItems
        }
      }}
  end.
