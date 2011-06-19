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

-record(irc_serverinfo, {servername, callback_module, motd}).
-record(irc_message, {prefix = false, command, params, trailing = false}).
-record(irc_user, {username, hostname, servername, realname}).
