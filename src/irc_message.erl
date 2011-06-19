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

-module(irc_message).

-export([parse/1, render/1]).
-export([reply/3, formatted_reply/3, formatted_reply/4]).

-include("irc.hrl").

%% <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
%% <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
%% <command>  ::= <letter> { <letter> } | <number> <number> <number>
%% <SPACE>    ::= ' ' { ' ' }
%% <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
%%
%% <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
%%                or NUL or CR or LF, the first of which may not be ':'>
%% <trailing> ::= <Any, possibly *empty*, sequence of octets not including
%%                  NUL or CR or LF>
%%
%% <crlf>     ::= CR LF

%% <target>     ::= <to> [ "," <target> ]
%% <to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
%% <channel>    ::= ('#' | '&') <chstring>
%% <servername> ::= <host>
%% <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
%% <nick>       ::= <letter> { <letter> | <number> | <special> }
%% <mask>       ::= ('#' | '$') <chstring>
%% <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and
%%                   comma (',')>

%% <user>       ::= <nonwhite> { <nonwhite> }
%% <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
%% <number>     ::= '0' ... '9'
%% <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'

%% <nonwhite>   ::= <any 8bit code except SPACE (0x20), NUL (0x0), CR
%%                   (0xd), and LF (0xa)>

parse(Line) ->
    L = re:replace(Line, "[\r\n]*\$", "", [{return, list}]),
    parse_words(string:tokens(L, " ")).

parse_words([":" ++ Prefix | Rest]) ->
    P = parse_words(Rest),
    P#irc_message{prefix = Prefix};
parse_words([Command | Rest]) ->
    {Params, Trailing} = parse_params(Rest, []),
    #irc_message{prefix = false,
                 command = Command,
                 params = Params,
                 trailing = Trailing}.

parse_params([":" ++ FirstTrailing | Rest], ParamsRev) ->
    {lists:reverse(ParamsRev), string:join([FirstTrailing | Rest], " ")};
parse_params([], ParamsRev) ->
    {lists:reverse(ParamsRev), false};
parse_params([Param | Rest], ParamsRev) ->
    parse_params(Rest, [Param | ParamsRev]).

render(M = #irc_message{prefix = false}) ->
    render_message(M);
render(M = #irc_message{prefix = Prefix}) ->
    [":", Prefix, " ", render_message(M)].

render_message(#irc_message{command = Command, params = Params, trailing = Trailing}) ->
    [stringify(Command),
     case Params of
         [] -> "";
         _ -> [" ", string:join([stringify(P) || P <- Params], " ")]
     end,
     case Trailing of
         false -> "";
         _ -> [" :", Trailing]
     end,
     "\r\n"].

stringify(N) when is_integer(N) ->
    integer_to_list(N);
stringify(S) when is_list(S) ->
    S.

%%---------------------------------------------------------------------------

reply('ERR_NOSUCHNICK', Target, [Nick]) ->
    formatted_reply(401, [Target, Nick], "No such nick/channel");

reply('ERR_NOSUCHSERVER', Target, [ServerName]) ->
    formatted_reply(402, [Target, ServerName], "No such server");

reply('ERR_NOSUCHCHANNEL', Target, [ChannelName]) ->
    formatted_reply(403, [Target, ChannelName], "No such channel");

reply('ERR_CANNOTSENDTOCHAN', Target, [ChannelName]) ->
    formatted_reply(404, [Target, ChannelName], "Cannot send to channel");

reply('ERR_TOOMANYCHANNELS', Target, [ChannelName]) ->
    formatted_reply(405, [Target, ChannelName], "You have joined too many channels");

reply('ERR_WASNOSUCHNICK', Target, [Nickname]) ->
    formatted_reply(406, [Target, Nickname], "There was no such nickname");

reply('ERR_TOOMANYTARGETS', Target, []) ->
    formatted_reply(407, [Target], "Duplicate recipients. No message delivered");

reply('ERR_NOORIGIN', Target, []) ->
    formatted_reply(409, [Target], "No origin specified");

reply('ERR_NORECIPIENT', Target, [Command]) ->
    formatted_reply(411, [Target], "No recipient given (~s)", [Command]);

reply('ERR_NOTEXTTOSEND', Target, []) ->
    formatted_reply(412, [Target], "No text to send");

reply('ERR_NOTOPLEVEL', Target, [Mask]) ->
    formatted_reply(413, [Target, Mask], "No toplevel domain specified");

reply('ERR_WILDTOPLEVEL', Target, [Mask]) ->
    formatted_reply(414, [Target, Mask], "Wildcard in toplevel domain");

reply('ERR_UNKNOWNCOMMAND', Target, [Command]) ->
    formatted_reply(421, [Target, Command], "Unknown command");

reply('ERR_NOMOTD', Target, []) ->
    formatted_reply(422, [Target], "MOTD File is missing");

reply('ERR_NOADMININFO', Target, [Server]) ->
    formatted_reply(423, [Target, Server], "No administrative info available");

reply('ERR_FILEERROR', Target, [FileOp, File]) ->
    formatted_reply(424, [Target], "File error doing ~s on ~s", [FileOp, File]);

reply('ERR_NONICKNAMEGIVEN', Target, []) ->
    formatted_reply(431, [Target], "No nickname given");

reply('ERR_ERRONEUSNICKNAME', Target, [Nick]) ->
    formatted_reply(432, [Target, Nick], "Erroneus nickname");

reply('ERR_NICKNAMEINUSE', Target, [Nick]) ->
    formatted_reply(433, [Target, Nick], "Nickname is already in use");

reply('ERR_NICKCOLLISION', Target, [Nick]) ->
    formatted_reply(436, [Target, Nick], "Nickname collision KILL");

reply('ERR_USERNOTINCHANNEL', Target, [Nick, Channel]) ->
    formatted_reply(441, [Target, Nick, Channel], "They aren't on that channel");

reply('ERR_NOTONCHANNEL', Target, [Channel]) ->
    formatted_reply(442, [Target, Channel], "You're not on that channel");

reply('ERR_USERONCHANNEL', Target, [User, Channel]) ->
    formatted_reply(443, [Target, User, Channel], "is already on channel");

reply('ERR_NOLOGIN', Target, [User]) ->
    formatted_reply(444, [Target, User], "User not logged in");

reply('ERR_SUMMONDISABLED', Target, []) ->
    formatted_reply(445, [Target], "SUMMON has been disabled");

reply('ERR_USERSDISABLED', Target, []) ->
    formatted_reply(446, [Target], "USERS has been disabled");

reply('ERR_NOTREGISTERED', Target, []) ->
    formatted_reply(451, [Target], "You have not registered");

reply('ERR_NEEDMOREPARAMS', Target, [Command]) ->
    formatted_reply(461, [Target, Command], "Not enough parameters");

reply('ERR_ALREADYREGISTRED', Target, []) ->
    formatted_reply(462, [Target], "You may not reregister");

reply('ERR_NOPERMFORHOST', Target, []) ->
    formatted_reply(463, [Target], "Your host isn't among the privileged");

reply('ERR_PASSWDMISMATCH', Target, []) ->
    formatted_reply(464, [Target], "Password incorrect");

reply('ERR_YOUREBANNEDCREEP', Target, []) ->
    formatted_reply(465, [Target], "You are banned from this server");

reply('ERR_KEYSET', Target, [Channel]) ->
    formatted_reply(467, [Target, Channel], "Channel key already set");

reply('ERR_CHANNELISFULL', Target, [Channel]) ->
    formatted_reply(471, [Target, Channel], "Cannot join channel (+l)");

reply('ERR_UNKNOWNMODE', Target, [Char]) ->
    formatted_reply(472, [Target, Char], "is unknown mode char to me");

reply('ERR_INVITEONLYCHAN', Target, [Channel]) ->
    formatted_reply(473, [Target, Channel], "Cannot join channel (+i)");

reply('ERR_BANNEDFROMCHAN', Target, [Channel]) ->
    formatted_reply(474, [Target, Channel], "Cannot join channel (+b)");

reply('ERR_BADCHANNELKEY', Target, [Channel]) ->
    formatted_reply(475, [Target, Channel], "Cannot join channel (+k)");

reply('ERR_NOPRIVILEGES', Target, []) ->
    formatted_reply(481, [Target], "Permission Denied- You're not an IRC operator");

reply('ERR_CHANOPRIVSNEEDED', Target, [Channel]) ->
    formatted_reply(482, [Target, Channel], "You're not channel operator");

reply('ERR_CANTKILLSERVER', Target, []) ->
    formatted_reply(483, [Target], "You cant kill a server!");

reply('ERR_NOOPERHOST', Target, []) ->
    formatted_reply(491, [Target], "No O-lines for your host");

reply('ERR_UMODEUNKNOWNFLAG', Target, []) ->
    formatted_reply(501, [Target], "Unknown MODE flag");

reply('ERR_USERSDONTMATCH', Target, []) ->
    formatted_reply(502, [Target], "Cant change mode for other users");

reply('RPL_USERHOST', Target, Replies) ->
    formatted_reply(302, [Target], "[~s]", [string:join(Replies, " ")]);

reply('RPL_ISON', Target, Nicks) ->
    formatted_reply(303, [Target], "[~s]", [string:join(Nicks, " ")]);

reply('RPL_AWAY', Target, [Nick, AwayMessage]) ->
    formatted_reply(301, [Target, Nick], "~s", [AwayMessage]);

reply('RPL_UNAWAY', Target, []) ->
    formatted_reply(305, [Target], "You are no longer marked as being away");

reply('RPL_NOWAWAY', Target, []) ->
    formatted_reply(306, [Target], "You have been marked as being away");

reply('RPL_WHOISUSER', Target, [Nick, User, Host, RealName]) ->
    formatted_reply(311, [Target, Nick, User, Host, "*"], "~s", [RealName]);

reply('RPL_WHOISSERVER', Target, [Nick, Server, ServerInfo]) ->
    formatted_reply(312, [Target, Nick, Server], "~s", [ServerInfo]);

reply('RPL_WHOISOPERATOR', Target, [Nick]) ->
    formatted_reply(313, [Target, Nick], "is an IRC operator");

reply('RPL_WHOISIDLE', Target, [Nick, Integer]) ->
    formatted_reply(317, [Target, Nick, Integer], "seconds idle");

reply('RPL_ENDOFWHOIS', Target, [Nick]) ->
    formatted_reply(318, [Target, Nick], "End of /WHOIS list");

reply('RPL_WHOISCHANNELS', Target, [Nick, Channels]) ->
    formatted_reply(319, [Target, Nick], "~s", [string:join(Channels, " ")]);

reply('RPL_WHOWASUSER', Target, [Nick, User, Host, RealName]) ->
    formatted_reply(314, [Target, Nick, User, Host, "*"], "~s", [RealName]);

reply('RPL_ENDOFWHOWAS', Target, [Nick]) ->
    formatted_reply(369, [Target, Nick], "End of WHOWAS");

reply('RPL_LISTSTART', Target, []) ->
    formatted_reply(321, [Target], "Channel :Users  Name");

reply('RPL_LIST', Target, [Channel, NumVisible, Topic]) ->
    formatted_reply(322, [Target, Channel, NumVisible], "~s", [Topic]);

reply('RPL_LISTEND', Target, []) ->
    formatted_reply(323, [Target], "End of /LIST");

reply('RPL_CHANNELMODEIS', Target, [Channel, Mode, ModeParams]) ->
    formatted_reply(324, [Target, Channel, Mode, ModeParams], false);

reply('RPL_NOTOPIC', Target, [Channel]) ->
    formatted_reply(331, [Target, Channel], "No topic is set");

reply('RPL_TOPIC', Target, [Channel, Topic]) ->
    formatted_reply(332, [Target, Channel], "~s", [Topic]);

reply('RPL_INVITING', Target, [Channel, Nick]) ->
    formatted_reply(341, [Target, Channel, Nick], false);

reply('RPL_SUMMONING', Target, [User]) ->
    formatted_reply(342, [Target, User], "Summoning user to IRC");

reply('RPL_VERSION', Target, [Version, DebugLevel, Server, Comments]) ->
    formatted_reply(351, [Target, Version ++ "." ++ DebugLevel, Server], "~s", [Comments]);

reply('RPL_WHOREPLY', Target, [Channel, User, Host, Server, Nick, Flags, HopCount, RealName]) ->
    formatted_reply(352, [Target, Channel, User, Host, Server, Nick, Flags],
                    "~b ~s", [HopCount, RealName]);

reply('RPL_ENDOFWHO', Target, [Name]) ->
    formatted_reply(315, [Target, Name], "End of /WHO list");

reply('RPL_NAMREPLY', Target, [Channel, Nicks]) ->
    formatted_reply(353, [Target, "@", Channel], "~s ", [string:join(Nicks, " ")]);

reply('RPL_ENDOFNAMES', Target, [Channel]) ->
    formatted_reply(366, [Target, Channel], "End of /NAMES list");

reply('RPL_LINKS', Target, [Mask, Server, HopCount, ServerInfo]) ->
    formatted_reply(364, [Target, Mask, Server], "~b ~s", [HopCount, ServerInfo]);

reply('RPL_ENDOFLINKS', Target, [Mask]) ->
    formatted_reply(365, [Target, Mask], "End of /LINKS list");

reply('RPL_BANLIST', Target, [Channel, Banid]) ->
    formatted_reply(367, [Target, Channel, Banid], false);

reply('RPL_ENDOFBANLIST', Target, [Channel]) ->
    formatted_reply(368, [Target, Channel], "End of channel ban list");

reply('RPL_INFO', Target, [String]) ->
    formatted_reply(371, [Target], "~s", [String]);

reply('RPL_ENDOFINFO', Target, []) ->
    formatted_reply(374, [Target], "End of /INFO list");

reply('RPL_MOTDSTART', Target, [Server]) ->
    formatted_reply(375, [Target], "- ~s Message of the day - ", [Server]);

reply('RPL_MOTD', Target, [Text]) ->
    formatted_reply(372, [Target], "- ~s", [Text]);

reply('RPL_ENDOFMOTD', Target, []) ->
    formatted_reply(376, [Target], "End of /MOTD command");

reply('RPL_YOUREOPER', Target, []) ->
    formatted_reply(381, [Target], "You are now an IRC operator");

reply('RPL_REHASHING', Target, [ConfigFile]) ->
    formatted_reply(382, [Target, ConfigFile], "Rehashing");

reply('RPL_TIME', Target, [Server, ServerLocalTimeString]) ->
    formatted_reply(391, [Target, Server], "~s", [ServerLocalTimeString]);

reply('RPL_USERSSTART', Target, []) ->
    formatted_reply(392, [Target], "UserID   Terminal  Host");

reply('RPL_USERS', Target, [UserID, Terminal, Host]) ->
    formatted_reply(393, [Target], "~-8s ~-9s ~-8s", [UserID, Terminal, Host]);

reply('RPL_ENDOFUSERS', Target, []) ->
    formatted_reply(394, [Target], "End of users");

reply('RPL_NOUSERS', Target, []) ->
    formatted_reply(395, [Target], "Nobody logged in").


formatted_reply(Code, Params, false) ->
    formatted_reply(Code, Params, false, []);
formatted_reply(Code, Params, Trailing) ->
    formatted_reply(Code, Params, "~s", [Trailing]).

formatted_reply(Code, Params, false, []) ->
    #irc_message{prefix = false,
                 command = Code,
                 params = Params,
                 trailing = false};
formatted_reply(Code, Params, FormatString, FormatArgs) ->
    #irc_message{prefix = false,
                 command = Code,
                 params = Params,
                 trailing = lists:flatten(io_lib:format(FormatString, FormatArgs))}.
