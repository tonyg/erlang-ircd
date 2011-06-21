%% Copyright (C) 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>
%%
%% This file is part of erlang-ircd.
%%
%% erlang-ircd is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published
%% by the Free Software Foundation, either version 3 of the License,
%% or (at your option) any later version.
%%
%% erlang-ircd is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with erlang-ircd.  If not, see <http://www.gnu.org/licenses/>.

-module(ircd_session).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-export([send/2, privmsg/4]).

-include("irc.hrl").

-record(state, {server_info, socket, nick, user, callback_state}).

%%---------------------------------------------------------------------------

send(Pid, Message = #irc_message{}) ->
    gen_server:cast(Pid, {send, Message}).

privmsg(Pid, Prefix, Targets, Text) ->
    send(Pid, #irc_message{prefix = Prefix,
                           command = "PRIVMSG",
                           params = [string:join(Targets, ",")],
                           trailing = Text}).

%%---------------------------------------------------------------------------

internal_send(Socket, Message) ->
    error_logger:info_report({sending, lists:flatten(irc_message:render(Message))}),
    gen_tcp:send(Socket, irc_message:render(Message)),
    ok.

reply(#state{socket = Socket, nick = Nick}, Type, Params) ->
    internal_send(Socket, irc_message:reply(Type, Nick, Params)).

callback(State = #state{server_info = #irc_serverinfo{callback_module = Mod},
                        callback_state = OldCS}, Op, Args) ->
    case OldCS of
        undefined when Op =/= login ->
            {ok, State}; %% TODO: better workflow
        _ ->
            case Mod:ircd_callback(Op, Args, OldCS) of
                {Result, NewCS} ->
                    {Result, State#state{callback_state = NewCS}}
            end
    end.

callback_discarding_result(State, Op, Args) ->
    {_Result, NewState} = callback(State, Op, Args),
    NewState.

maybe_login(State = #state{nick = N, user = U, callback_state = undefined})
  when N =/= undefined andalso U =/= undefined ->
    send_motd(State),
    callback_discarding_result(State, login, [N, U]);
maybe_login(State) ->
    State.

send_motd(State = #state{server_info = #irc_serverinfo{servername = ServerName,
                                                       motd = Motd}}) ->
    reply(State, 'RPL_MOTDSTART', [ServerName]),
    [reply(State, 'RPL_MOTD', [Line]) || Line <- Motd],
    reply(State, 'RPL_ENDOFMOTD', []),
    ok.

handle_irc_message(#irc_message{command = "NICK", params = [Nick]},
                   State) ->
    {noreply, callback_discarding_result(maybe_login(State#state{nick = Nick}),
                                         nick_change, [Nick])};
handle_irc_message(#irc_message{command = "USER", params = [U, H, S], trailing = R},
                   State) ->
    User = #irc_user{username = U, hostname = H, servername = S, realname = R},
    {noreply, callback_discarding_result(maybe_login(State#state{user = User}),
                                         user_change, [User])};
handle_irc_message(#irc_message{command = "QUIT"},
                   State) ->
    {stop, normal, disconnect(State)};
handle_irc_message(#irc_message{command = "JOIN", params = [Channels | MaybeKeys]},
                   State = #state{nick = Nick, socket = Sock}) ->
    {ChannelInfo, NewState} =
        callback(State, join, [string:tokens(Channels, ","),
                               case MaybeKeys of
                                   [] -> [];
                                   [Keys] -> string:tokens(Keys, ",")
                               end]),
    [begin
         internal_send(Sock, irc_message:reply('RPL_NAMREPLY', Nick, [Channel, Names])),
         internal_send(Sock, irc_message:reply('RPL_ENDOFNAMES', Nick, [Channel])),
         internal_send(Sock, irc_message:reply('RPL_TOPIC', Nick, [Channel, Topic]))
     end || {Channel, Names, Topic} <- ChannelInfo],
    {noreply, NewState};
handle_irc_message(#irc_message{command = "PART", params = [Channels]},
                   State) ->
    {noreply, callback_discarding_result(State, part, [string:tokens(Channels, ",")])};
handle_irc_message(#irc_message{command = "PRIVMSG", params = [Targets], trailing = Text},
                   State) ->
    {noreply, callback_discarding_result(State, privmsg, [string:tokens(Targets, ","), Text])};
handle_irc_message(Msg,
                   State) ->
    error_logger:info_report({ignored_command, Msg}),
    {noreply, State}.

disconnect(State = #state{socket = Sock, callback_state = OldCS}) ->
    case Sock of
        undefined -> ok;
        _ -> gen_tcp:close(Sock)
    end,
    case OldCS of
        undefined ->
            State#state{socket = undefined};
        _ ->
            S = callback_discarding_result(State#state{socket = undefined}, disconnect, []),
            S#state{callback_state = undefined}
    end.

%%---------------------------------------------------------------------------

init([Sock, ServerInfo]) ->
    {ok, #state{server_info = ServerInfo, socket = Sock, callback_state = undefined}}.

terminate(_Reason, State) ->
    disconnect(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(_Request, _From, State) ->
    {stop, {bad_call, _Request}, State}.

handle_cast({socket_control_transferred, Sock}, State = #state{socket = Sock}) ->
    {noreply, State};
handle_cast({send, Message = #irc_message{}}, State = #state{socket = Sock}) ->
    internal_send(Sock, Message),
    {noreply, State};
handle_cast(_Request, State) ->
    {stop, {bad_cast, _Request}, State}.

handle_info({tcp, Sock, Line}, State = #state{socket = Sock}) ->
    handle_irc_message(irc_message:parse(Line), State);
handle_info(_Message, State) ->
    {stop, {bad_info, _Message}, State}.
