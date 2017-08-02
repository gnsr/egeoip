-module(egeoip_db_updater).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(REFRESH_INTERVAL_MS, timer:hours(12)).
-define(REFRESH_FROM_REMOTE_TIMEOUT_MS, timer:seconds(15)).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    dblocation :: dblocation(),
    dbpath :: string(),
    db :: binary()
}).
-type state() :: #state{}.

-type dblocation() :: local | remote.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(DbLocAndPath :: undefined
                         | string())
      -> ignore | {ok, pid()}.
start_link(undefined) -> ignore;
start_link(DbLocAndPath) -> gen_server:start_link(?CB_MODULE, [DbLocAndPath], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([DbLocAndPath :: {dblocation(), string()}])
      -> {ok, State :: state()}.
init([{DbLocation, DbPath}]) ->
    erlang:send_after(0, self(), reload_db),
    {ok, #state{ dblocation = DbLocation, dbpath = DbPath }}.

-spec terminate(Reason :: term(),
                State :: state())
      -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term(),
                  State :: state(),
                  Extra :: term())
      -> {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_call(Request :: term(),
                  From :: {Pid :: pid(), Tag :: binary()},
                  State :: state())
      -> {noreply, State :: state()}
       | {reply, Reply :: term(), State :: state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(Request :: term(),
                  State :: state())
      -> {noreply, NewState :: state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: reload
                        | term(),
                  State :: state())
      -> {noreply, NewState :: state()}.
handle_info(reload_db, #state{ dblocation = DbLoc, dbpath = DbPath } = State) ->
    NewState = maybe_reload_db_from_location(DbLoc, DbPath, State),
    erlang:send_after(?REFRESH_INTERVAL_MS, self(), reload_db),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec maybe_reload_db_from_location(DbLoc :: dblocation(),
                                    DbPath :: string(),
                                    State :: state())
      -> NewState :: state().
maybe_reload_db_from_location(remote, DbPath, State) ->
    case httpc:request(get, {DbPath, []}, [{timeout, ?REFRESH_FROM_REMOTE_TIMEOUT_MS}], []) of
        {error, _Reason} ->
            State;
        {ok, {_, _, DatGZ}} ->
            maybe_reload_db(DatGZ, State)
    end;
maybe_reload_db_from_location(local, DbPath, State) ->
    case file:read_file(DbPath) of
        {error, _Reason} ->
            State;
        {ok, DatGZ} ->
            maybe_reload_db(DatGZ, State)
    end.

-spec maybe_reload_db(DatGZ :: binary(),
                      State :: state())
      -> NewState :: state().
maybe_reload_db(DatGZ, State) when DatGZ =/= State#state.db ->
    _ = egeoip:reload({compressed_data, DatGZ}),
    State#state{ db = DatGZ };
maybe_reload_db(_DatGZ, State) ->
    State.
