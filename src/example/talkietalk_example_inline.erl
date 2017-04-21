%%%-------------------------------------------------------------------
%%% @author mrchex
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2017 12:54 PM
%%%-------------------------------------------------------------------
-module(talkietalk_example_inline).
-author("mrchex").

%% API
-export([
  handle/3
]).

handle(query, #{query := Query, offset := Offset} = Q, State)->
  io:format("Inline query: ~p~n", [Q]),

  Results = [
    #{type => article,
      id => <<"1-id">>,
      title => <<"Title">>,
      description => <<"Some description">>,
      input_message_content => #{
        message_text => <<"message text">>
      }}
  ],

  talkietalk_telegram:request(<<"answerInlineQuery">>, #{
    inline_query_id => maps:get(id, Q),
    is_personal => true,
    results => Results
  }),

  ignore.
