-module (counter_srv).

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

-define (DELAY_DEF, 60 * 1000000). % sec -> microseconds

-type start_error() :: {already_started, pid()} | term().
-type timestamp_list() :: [] | list(erlang:timestamp()).


-record (state, {delay :: integer (),
                 list  :: timestamp_list()
                }).


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

-spec incr () -> ok.
incr () ->
  gen_server:call (?MODULE, {incr, erlang:timestamp()}).

-spec get_value () -> integer().
get_value () ->
  gen_server:call (?MODULE, {get_value, erlang:timestamp()}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init ([]) ->
  {ok, #state{ list  = [],
               delay = ?DELAY_DEF }}.

handle_call ({incr, Time}, _From, #state{ list = CounterList }=State) ->
  {reply, ok, State#state{ list = [Time | CounterList ] }};

handle_call ({get_value, TimeNow}, _From, #state{ delay = Delay,
                                                  list = CounterList }=State) ->
  Value = count_value (CounterList, TimeNow, Delay),
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

-spec count_value (
        TimeList :: timestamp_list(),
        TimeNow  :: erlang:timestamp(),
        Delay    :: integer()
       ) -> integer().
count_value (TimeList, TimeNow, Delay) ->
  F = fun
        ({_, _, _}=Timestamp, Acc) ->
          case timer:now_diff (TimeNow, Timestamp) of
            Diff when Diff  > Delay  -> Acc;
            _Else                    -> Acc + 1
          end;
        (BadElement, Acc) ->
          ?PRINT ("Error: wrong format of timestamp = ~p~n", [BadElement]),
          Acc
      end,
  lists:foldl (F, 0, TimeList).
