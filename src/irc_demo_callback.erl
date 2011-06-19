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

-module(irc_demo_callback).

-include("irc.hrl").

-export([ircd_callback/3]).

-record(state, {session_pid, nick}).

ircd_callback(login, [Nick, _User], undefined) ->
    SessionPid = self(),
    {noreply, spawn_link(fun () -> main(#state{session_pid = SessionPid,
                                               nick = Nick}) end)};
ircd_callback(Op, Args, Pid) ->
    Pid ! {ircd_callback, Op, Args},
    {noreply, Pid}.

main(State) ->
    receive
        {ircd_callback, disconnect, _} ->
            error_logger:info_report({bye}),
            ok;
        {ircd_callback, Op, Args} ->
            main(handle_callback(Op, Args, State))
    after 3000 ->
            ircd_session:privmsg(State#state.session_pid,
                                 "someone",
                                 ["#room"],
                                 io_lib:format("Hi ~p", [erlang:now()])),
            main(State)
    end.

handle_callback(join, [Channels, _Keys], State = #state{session_pid = SessionPid,
                                                        nick = Nick}) ->
    [begin
         ircd_session:send(SessionPid,
                           #irc_message{prefix = Nick,
                                        command = "JOIN",
                                        params = [Channel]}),
         ircd_session:send(SessionPid,
                           irc_message:reply('RPL_NAMREPLY', Nick,
                                             [Channel, [Nick, "otherguy"]])),
         ircd_session:send(SessionPid,
                           irc_message:reply('RPL_ENDOFNAMES', Nick, [Channel])),
         ircd_session:send(SessionPid,
                           irc_message:reply('RPL_TOPIC', Nick,
                                             [Channel, "This is the topic"]))
     end || Channel <- Channels],
    State;
handle_callback(part, [Channels], State = #state{session_pid = SessionPid,
                                                 nick = Nick}) ->
    [begin
         ircd_session:send(SessionPid,
                           #irc_message{prefix = Nick,
                                        command = "PART",
                                        params = [Channel]})
     end || Channel <- Channels],
    State;
handle_callback(privmsg, [Targets, Text], State = #state{session_pid = SessionPid,
                                                         nick = Nick}) ->
    [begin
         ircd_session:privmsg(SessionPid, Nick, [Target], Text)
     end || Target <- Targets],
    State;
handle_callback(Op, Args, State) ->
    error_logger:info_report({unhandled_ircd_callback, Op, Args, State}),
    State.
