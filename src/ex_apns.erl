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

%% @type env() = production | development.
%% @type start_error() = {error, {already_started, pid()} | term()}.
%% @type server_ref() = Name::atom() | {Name::atom(), Node::atom()} | pid().
%% @type token() = iodata() | integer().
%% @type payload() = mochijson2:json_object() | iodata().
%% @type feedback() = [{token(), Timestamp::integer()}].

-module(ex_apns).
-behaviour(gen_server).
-author('Anthony Ramine <nox@dev-extend.eu>').

-export([start/3,
         start_link/3,
         send/3,
         feedback/1,
         token_to_integer/1,
         token_to_binary/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {env, certfile, socket}).


%% @spec start(atom(), env(), string()) -> {ok, Pid} | start_error()
%% @doc Create an ex_apns process.
%%      The resulting process will be locally registered as `Name'.
start(Name, Env, CertFile) ->
  ex_apns_sup:start_child(Name, Env, CertFile).

%% @spec start_link(atom(), env(), string()) -> {ok, Pid} | start_error()
%% @doc Create an ex_apns process as part of a supervision tree.
%%      The resulting process will be locally registered as `Name'.
start_link(Name, Env, CertFile) ->
  gen_server:start_link({local, Name}, ?MODULE, {Env, CertFile},
                        [{timeout, infinity}]).

%% @spec send(server_ref(), token(), payload()) -> ok
%% @doc Send a notification.
send(ServerRef, Token, Payload) ->
  gen_server:cast(ServerRef, {send, Token, Payload}).

%% @spec feedback(server_ref()) -> feedback() | {error, term()}
%% @doc Retrieve feedback of a given ex_apns process.
feedback(ServerRef) ->
  case gen_server:call(ServerRef, feedback_socket, infinity) of
    {ok, Socket} -> feedback_loop(Socket);
    Error -> Error end.

%% @spec token_to_integer(Token::token()) -> integer()
%% @doc Convert a token to its integer representation.
token_to_integer(Bin) when is_binary(Bin), byte_size(Bin) =:= 64 ->
  token_to_integer(Bin, 0);
token_to_integer(I) when is_integer(I), I >= 0, I < 1 bsl 256 ->
  I;
token_to_integer(List) when is_list(List) ->
  token_to_integer(iolist_to_binary(List)).

%% @spec token_to_binary(Token::token()) -> binary()
%% @doc Convert a token to its binary representation.
token_to_binary(I) when is_integer(I), I >= 0, I < 1 bsl 256 ->
  iolist_to_binary(erlang:integer_to_list(I, 16));
token_to_binary(Bin) when is_binary(Bin), byte_size(Bin) =:= 64 ->
  Bin;
token_to_binary(List) when is_list(List) ->
  token_to_binary(iolist_to_binary(List)).


%% @hidden
init({Env, CertFile}) ->
  case connect(env_to_gateway(Env), 2195, CertFile) of
    {ok, Socket} ->
      {ok, #state{env = Env, certfile = CertFile, socket = Socket}};
    {error, Reason} -> {stop, Reason} end.

%% @hidden
handle_call(feedback_socket, _From,
            State = #state{env = Env, certfile = CertFile}) ->
  {reply, connect(env_to_feedback(Env), 2196, CertFile), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast({send, Token, Payload}, State = #state{socket = Socket}) ->
  TokenInt = token_to_integer(Token),
  PayloadBin = payload_to_binary(Payload),
  Packet = [<<0, 32:16, TokenInt:256,
            (iolist_size(PayloadBin)):16>> | PayloadBin],
  ok = ssl:send(Socket, Packet),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Msg, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @spec connect(address(), integer(), string()) -> result()
%%       where address() = string() | atom() | inet:ip_address()
%%             result() = {ok, ssl:socket()} | {error, inet:posix()}
connect(Address, Port, CertFile) ->
  CaCertFile = filename:join([code:priv_dir(?MODULE), "entrust_ev_ca.cer"]),
  SslOptions = [binary,
                {active, false},
                {certfile, CertFile},
                {cacertfile, CaCertFile},
                {ssl_imp, old}],
  ssl:connect(Address, Port, SslOptions).

%% @spec feedback_loop(ssl:socket()) -> feedback() | {error, reason()}
%%       where reason() = {bad_recv, binary()} | term()
feedback_loop(Socket) ->
  feedback_loop(Socket, []).

%% @hidden
feedback_loop(Socket, Acc) ->
  case ssl:recv(Socket, 39, infinity) of
    {ok, <<8, Timestamp:32, 32:16, Token:256>>} ->
      feedback_loop(Socket, [{Token, Timestamp} | Acc]);
    {error, closed} -> lists:reverse(Acc);
    Error -> Error end.

%% @spec env_to_gateway(Env::env()) -> atom()
env_to_gateway(production) ->
  'gateway.push.apple.com';
env_to_gateway(development) ->
  'gateway.sandbox.push.apple.com'.

%% @spec env_to_feedback(Env::env()) -> atom()
env_to_feedback(production) ->
  'feedback.push.apple.com';
env_to_feedback(development) ->
  'feedback.sandbox.push.apple.com'.

%% @hidden
token_to_integer(<<C, Rest/binary>>, I) when C >= $0, C =< $9 ->
  token_to_integer(Rest, I * 16 + C - $0);
token_to_integer(<<C, Rest/binary>>, I) when C >= $a, C =< $f ->
  token_to_integer(Rest, I * 16 + C - $a + 10);
token_to_integer(<<C, Rest/binary>>, I) when C >= $A, C =< $F ->
  token_to_integer(Rest, I * 16 + C - $A + 10);
token_to_integer(<<>>, I) ->
  I.

%% @spec payload_to_binary(Payload::payload()) -> iodata()
payload_to_binary(Json = {struct, _Properties}) ->
  mochijson2:encode(Json);
payload_to_binary(Bin) when is_binary(Bin) ->
  Bin;
payload_to_binary(List) when is_list(List) ->
  List.
