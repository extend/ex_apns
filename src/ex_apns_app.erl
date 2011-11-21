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

-module(ex_apns_app).
-behaviour(application).
-author('Anthony Ramine <n.oxyde@gmail.com>').

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
