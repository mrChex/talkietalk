-module(talkietalk_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(TalkModule, TelegramToken) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [TalkModule, TelegramToken]).

init([TalkModule, TelegramToken]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, [
    ?CHILD(talkietalk_talkie, worker, [TalkModule]),
    ?CHILD(talkietalk_telegram, worker, [TelegramToken])
  ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
