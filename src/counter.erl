-module (counter).

-behaviour (gen_server).

%% API
-export ([start_link/0]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

%% MAIN API
-export ([incr/0,
          get_value/0]).


-define (PRINT (F, A),
         io:format (
           "~s.erl(~p): ~s/~p -> ~s", 
           [?MODULE, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, io_lib:format(F, A)]
          )).
-define (PRINT (F), ?PRINT (F, [])).

-define (DELAY, 60 * 1000000). % sec -> microseconds

-type start_error() :: {already_started, pid()} | term().


-record (state, {list :: [] | list(erlang:timestamp())}).


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the server as part of a supervision tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link () -> {ok, pid()} | {error, start_error()} | ignore.
start_link() ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

incr () ->
  gen_server:call (?MODULE, {incr, erlang:timestamp()}).

get_value () ->
  gen_server:call (?MODULE, get_value).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init ([]) ->
  {ok, #state{ list = [] }}.

handle_call ({incr, Time}, _From, #state{ list = CounterList }=State) ->
  %?PRINT ("incr() from ~p, with Time = ~p~n", [_From, Time]),
  {reply, ok, State#state{ list = [Time | CounterList ] }};

handle_call (get_value, _From, #state{ list = CounterList }=State) ->
  %?PRINT ("get_value() from ~p~n", [_From]),
  Value = count_value (CounterList),
  {reply, Value, State};

handle_call (_Request, _From, State) ->
  ?PRINT ("Undandled REQUEST!~n"),
  {reply, ignored, State}.

handle_cast (_Msg, State) ->
  ?PRINT ("Undandled MSG!~n"),
  {noreply, State}.

handle_info (_Info, State) ->
  ?PRINT ("Undandled INFO!~n"),
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

count_value (TimeList) ->
  TimeNow = erlang:timestamp(),
  F = fun
        ([H | _T], Acc) ->
          case timer:now_diff (TimeNow, H) > ?DELAY of
            true -> Acc;
            false -> Acc + 1
          end;
        (_, _Acc) -> 0
      end,
  lists:foldl (F, 0, TimeList).
