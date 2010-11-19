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

-module(ex_apns_app).
-behaviour(application).
-author('Anthony Ramine <nox@dev-extend.eu>').

-export([start/0]).

-export([start/2,
         stop/1]).


%% @spec start() -> ok | {error, term()}
%% @doc Start ex_apns application and its dependencies.
start() ->
  ensure([crypto, public_key, ssl, ex_apns]).


%% @hidden
start(_StartType, _StartArgs) ->
  ex_apns_sup:start_link().

%% @hidden
stop(_State) ->
  ok.


%% @spec ensure([atom()]) -> ok | {error, term()}
ensure([App | Apps]) ->
  case application:start(App) of
    ok -> ensure(Apps);
    {error, {already_started, App}} -> ensure(Apps);
    Error -> Error end;
ensure([]) ->
  ok.
