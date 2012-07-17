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

-export([start/0, ircd_callback/3]).

-record(state, {session_pid, nick}).
-record(ch, {name, members}).
-record(binding, {nick, pid, ref}).

-define(CHANNELS_TABLE, ircd_channels).

%%---------------------------------------------------------------------------

start() ->
    ?CHANNELS_TABLE = ets:new(?CHANNELS_TABLE, [named_table, public]),
    ok.

%%---------------------------------------------------------------------------

get_channel(Name) ->
    case ets:lookup(?CHANNELS_TABLE, Name) of
        [{Name, Pid}] ->
            Pid;
        [] ->
            Pid = spawn(fun () -> channel_boot(Name) end),
            case ets:insert_new(?CHANNELS_TABLE, {Name, Pid}) of
                true ->
                    Pid ! start,
                    Pid;
                false ->
                    Pid ! stop,
                    get_channel(Name)
            end
    end.

deregister_channel({Name, Pid}) ->
    ets:delete_object(?CHANNELS_TABLE, {Name, Pid}),
    ok.

broadcast(#ch{name = Name, members = Members}, Event) ->
    Message = {channel_event, Name, Event},
    error_logger:info_report({broadcast, Name, Members, Message}),
    [Pid ! Message || #binding{pid = Pid} <- Members],
    ok.

channel_boot(Name) ->
    receive
        start ->
            channel_main(#ch{name = Name,
                             members = []});
        stop ->
            ok
    after 1000 ->
            deregister_channel({Name, self()})
    end.

channel_main(Ch = #ch{name = Name, members = OldMembers}) ->
    receive
        {join, Nick, Pid} ->
            Ref = erlang:monitor(process, Pid),
            NewMembers = [#binding{nick = Nick, pid = Pid, ref = Ref}
                          | lists:keydelete(Nick, #binding.nick, OldMembers)],
            NewCh = Ch#ch{members = NewMembers},
            ok = broadcast(NewCh, {join, Nick}),
            channel_main(NewCh);
        {part, Nick} ->
            [erlang:demonitor(Ref)
             || #binding{nick = N, ref = Ref} <- OldMembers, N == Nick],
            NewMembers = lists:keydelete(Nick, #binding.nick, OldMembers),
            ok = broadcast(Ch, {part, Nick}),
            channel_main(Ch#ch{members = NewMembers});
        {privmsg, Nick, Text} ->
            ok = broadcast(Ch, {privmsg, Nick, Text}),
            channel_main(Ch);
        {request, ReplyPid, Ref, info} ->
            ReplyPid ! {reply, Ref, {channel_info, Name, [{member_count = length(OldMembers)}]}},
            channel_main(Ch);
        {request, ReplyPid, Ref, members} ->
            ReplyPid ! {reply, Ref, {channel_members, Name, OldMembers}},
            channel_main(Ch);
        {'DOWN', Ref, process, _Pid, _ExitReason} ->
            [ok = broadcast(Ch, {part, Nick})
             || #binding{nick = Nick, ref = R} <- OldMembers, R == Ref],
            NewMembers = lists:keydelete(Ref, #binding.ref, OldMembers),
            channel_main(Ch#ch{members = NewMembers})
    end.

channel_rpc(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {request, self(), Ref, Request},
    receive
        {reply, Ref, Reply} ->
            {ok, Reply}
    after 30000 ->
            error_logger:error_report({channel_rpc, Request, timeout}),
            {error, timeout}
    end.

%%---------------------------------------------------------------------------

ircd_callback(login, [Nick, _User], undefined) ->
    SessionPid = self(),
    {ok, spawn_link(fun () -> main(#state{session_pid = SessionPid,
                                          nick = Nick}) end)};
ircd_callback(Op, Args, Pid) ->
    Ref = make_ref(),
    Pid ! {ircd_callback, Ref, self(), Op, Args},
    receive
        {stopped, Ref, Value} ->
            {Value, undefined};
        {running, Ref, Value} ->
            {Value, Pid}
    after 30000 ->
            exit({?MODULE, ircd_callback, timeout})
    end.

main(State) ->
    receive
        {ircd_callback, Ref, ReplyPid, Op, Args} ->
            case Op of
                disconnect ->
                    error_logger:info_report({bye}),
                    ReplyPid ! {stopped, Ref, ok};
                _ ->
                    {Reply, NewState} = handle_callback(Op, Args, State),
                    ReplyPid ! {running, Ref, Reply},
                    main(NewState)
            end;
        {channel_event, Channel, Event} ->
            main(handle_channel_event(Channel, Event, State));
        Other ->
            error_logger:error_report({irc_demo_callback, received_martian, Other}),
            exit({irc_session_process_received, Other})
    end.

handle_callback(join, [Channels, _Keys], State = #state{nick = Nick}) ->
    {[begin
          Pid = get_channel(Channel),
          Pid ! {join, Nick, self()},
          {ok, {channel_members, Channel, Bindings}} = channel_rpc(Pid, members),
          {Channel, [N || #binding{nick = N} <- Bindings], "Topics not yet supported"}
      end || Channel <- Channels], State};
handle_callback(part, [Channels], State = #state{nick = Nick}) ->
    [begin
         Pid = get_channel(Channel),
         Pid ! {part, Nick}
     end || Channel <- Channels],
    {ok, State};
handle_callback(privmsg, [Targets, Text], State = #state{nick = Nick}) ->
    [begin
         Pid = get_channel(Target),
         Pid ! {privmsg, Nick, Text}
     end || Target <- Targets, is_channel_name(Target)],
    {ok, State};
handle_callback(Op, Args, State) ->
    error_logger:info_report({unhandled_ircd_callback, Op, Args, State}),
    {ok, State}.

is_channel_name("#" ++ _) -> true;
is_channel_name(_) -> false.

handle_channel_event(Channel, {join, OtherNick}, State = #state{session_pid = SessionPid}) ->
    ircd_session:send(SessionPid, #irc_message{prefix = OtherNick,
                                               command = "JOIN",
                                               params = [],
                                               trailing = Channel}),
    State;
handle_channel_event(Channel, {part, OtherNick}, State = #state{session_pid = SessionPid}) ->
    ircd_session:send(SessionPid, #irc_message{prefix = OtherNick,
                                               command = "PART",
                                               params = [],
                                               trailing = Channel}),
    State;
handle_channel_event(Channel, {privmsg, OtherNick, Text},
                     State = #state{nick = Nick,
                                    session_pid = SessionPid}) ->
    if
        OtherNick == Nick ->
            State;
        true ->
            ircd_session:privmsg(SessionPid, OtherNick, [Channel], Text),
            State
    end.
