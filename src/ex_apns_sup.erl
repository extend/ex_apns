%% Copyright (c) 2010, Dev:Extend
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%  * Neither the name of Dev:Extend nor the names of its contributors may be
%%    used to endorse or promote products derived from this software without
%%    specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% @type env() = ex_apns:env().
%% @type start_error() = ex_apns:start_error().

-module(ex_apns_sup).
-behaviour(supervisor).
-author('Anthony Ramine <nox@dev-extend.eu>').

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
