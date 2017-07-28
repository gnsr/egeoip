%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

-module(egeoip_sup).
-author('bob@redivi.com').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([worker/2, worker_names/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    File = case application:get_env(egeoip, dbfile) of
	       {ok, Other} ->
		   Other;
	       _ ->
		   city
	   end,
    Processes = worker(tuple_to_list(worker_names()), File),
    DbToUpdate = application:get_env(egeoip, dbpath, undefined),
    Updater = [{egeoip_db_updater, {egeoip_db_updater, start_link, [DbToUpdate]},
                permanent, 5000, worker, [egeoip_db_updater]}],
    {ok, {{one_for_one, 5, 300}, Processes ++ Updater}}.

worker_names() ->
    {egeoip_0,
     egeoip_1,
     egeoip_2,
     egeoip_3,
     egeoip_4,
     egeoip_5,
     egeoip_6,
     egeoip_7}.

worker([], _File) ->
    [];
worker([Name | T], File) ->
    [{Name,
      {egeoip, start_link, [Name, File]},
      permanent, 5000, worker, [egeoip]} | worker(T, File)].
