%% Copyright (c) 2011, Anthony Ramine <n.oxyde@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @type env() = ex_apns:env().
%% @type start_error() = ex_apns:start_error().

-module(ex_apns_sup).
-behaviour(supervisor).
-author('Anthony Ramine <n.oxyde@gmail.com>').

-export([start_link/0,
         start_child/3]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% @spec start_link() -> {ok, Pid} | start_error()
%% @doc Start ex_apns supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec start_child(atom(), env(), string()) -> {ok, Pid} | start_error()
%% @doc Create an ex_apns child process.
%%      The resulting process will be locally registered as `Name'.
start_child(Name, Env, CertFile) ->
  supervisor:start_child(?MODULE, [Name, Env, CertFile]).


%% @hidden
init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, [?CHILD(ex_apns, worker)]}}.
