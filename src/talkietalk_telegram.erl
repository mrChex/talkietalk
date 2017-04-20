-module(talkietalk_telegram).
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
  code_change/3
]).

%% api
-export([
%%  getUpdates/0,
  sendMessage/2,
  sendMessage/3,
  sendMessage/4,
  sendMarkdownMessage/2,
  sendMessage/5
%%  request/2
]).

-define(SERVER, ?MODULE).

-define(CHECK_UPDATES_DELAY, 2000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(TelegramToken) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [TelegramToken], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([TelegramToken]) ->

  LastUpdateId = 0,
  spawn_link(fun() -> checkUpdates(TelegramToken) end),

  {ok, #{
    last_update_id => LastUpdateId,
    telegram_token => TelegramToken
  }}.

handle_call(last_update_id, _, #{last_update_id := LastUpdateId} = State)->
  {reply, LastUpdateId, State};

handle_call({sendMessage, ChatId, Text, ReplyTo, ReplyMarkup, ParseMode}, From, #{telegram_token := Token} = State)->
  spawn(fun() ->
    Params0 = #{
      chat_id => ChatId,
      text => Text
    },
    Params1 = case ReplyTo of
      null -> Params0;
      _ -> maps:put(reply_to_message_id, ReplyTo, Params0)
    end,
    Params2 = case ReplyMarkup of
      null -> Params1;
      _ -> maps:put(reply_markup, ReplyMarkup, Params1)
    end,
    Params = case ParseMode of
      null -> Params2;
      _ -> maps:put(parse_mode, ParseMode, Params2)
    end,
    Reply = request(Token,<<"sendMessage">>, Params),
    gen_server:reply(From, Reply)
  end),
  {noreply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({update, #{ <<"message">> := Message, <<"update_id">> := UpdateId } = Update}, State) ->
  io:format("Received msg > ~p~n", [Message]),

  talkietalk_talkie:message(Message),

  {noreply, State#{
    last_update_id => UpdateId
  }};

handle_cast({update, #{ <<"edited_message">> := Message, <<"update_id">> := UpdateId } = Update}, State) ->
  io:format("Received edited msg > ~p~n", [Message]),

  #{<<"chat">> := #{<<"id">> := ChatId}} = Message,

  spawn(fun() ->
    sendMessage(ChatId, <<"О, отредактированное сообщение. Обработаю его еще раз."/utf8>>)
  end),
  talkietalk_talkie:message(Message),

  {noreply, State#{
    last_update_id => UpdateId
  }};


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

checkUpdates(Token) ->
  getUpdates(Token),
  timer:sleep(?CHECK_UPDATES_DELAY),
  checkUpdates(Token).

getUpdates(Token) ->
  LastUpdateId = gen_server:call(?SERVER, last_update_id),
  Updates = request(Token, <<"getUpdates">>, #{
    offset => LastUpdateId+1
  }),
  [gen_server:cast(?SERVER, {update, Update}) || Update <- Updates].

sendMessage(ChatId, Text) -> sendMessage(ChatId, Text, null, null, null).

sendMessage(ChatId, Text, ReplyTo) -> sendMessage(ChatId, Text, ReplyTo, null, null).

sendMessage(ChatId, Text, ReplyTo, ReplyMarkup) -> sendMessage(ChatId, Text, ReplyTo, ReplyMarkup, null).

sendMarkdownMessage(ChatId, Text) ->
  sendMessage(ChatId, Text, null, null, markdown).

sendMessage(ChatId, Text, ReplyTo, ReplyMarkup, ParseMode)->
  gen_server:call(?SERVER, {sendMessage, ChatId, Text, ReplyTo, ReplyMarkup, ParseMode}).

request(Token, Method, Params) when is_binary(Method), is_map(Params)->
  Uri = <<"https://api.telegram.org/bot", Token/binary, "/", Method/binary>>,
  Options = [ {timeout, infinity} ],
  RequestBody = jsx:encode(Params),

  {ok, {
    {"HTTP/1.1", 200, "OK"},
    _Headers,
    Body
  }} = httpc:request(post, {binary_to_list(Uri), [], "application/json", RequestBody}, Options,[]),

  ResponseJson = jsx:decode(list_to_binary(Body), [return_maps]),

  case ResponseJson of
    #{<<"ok">> := true, <<"result">> := Result} -> Result
  end.
