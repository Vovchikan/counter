%%%-------------------------------------------------------------------
%% @doc counter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(counter_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % стратегия перезапуска
%%                    one_for_one - если один дочерний процесс упадёт, то должен быть перезапущен только он;
%%                    one_for_all - если один дочерний процесс упадёт, то должны быть перезапущены все дочерние процессы;
%%                    rest_for_one - если один дочерний процесс упадёт, то должен быть перезапущен он и все процессы, которые были запущены после него;
%%                    simple_one_for_one - для динамических дочерних процессов.
%%                 intensity => non_neg_integer(), % макс число перезапусков
%%                 period => pos_integer()}        % макс время за которое может быть осуществленно макс число перезапусков
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % кортеж, который указывает как запустить дочерний процесс - {M, F, A}
%%                    M - имя модуля;
%%                    F - имя функциии, очень важно, чтобы функция была совместима с требованиями OTP и создавала связь с вызывающим процессом;
%%                    A - аргументы
%%                  restart => restart(),   % сообщает наблюдателю как реагировать на смерть дочернего процесса
%%                    permanent - постоянный, должен быть всегда перезапущен наблюдателем;
%%                    temporary - временный, никогда не должен быть перезапущен;
%%                    transient - краткосрочный, может быть перезапущен, только если завершится с ошибкой, инчане никогда не должен быть перезапущен;
%%                  shutdown => shutdown(), % время на выполнение terminate(), по истечению которого незавершённый процесс будет убить через exit (Pid, kill)
%%                  type => worker(),       % дочерний процесс либо тоже наблюдатель, либо обычный работяга
%%                    worker - работяга;
%%                    supervisor - супервизор;
%%                  modules => modules()}   % список состоящий из одного элемента - имя модуля обратного вызова ???
init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 3,
               period => 1000},
  ChildSpecs = [ #{id       => counter_srv,
                   start    => {counter_srv, start_link, []},
                   restart  => transient,
                   type     => worker,
                   modules  => [counter_srv]}
               ],

  {ok, {SupFlags, ChildSpecs}}.
