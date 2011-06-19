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

-module(ircd_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    application:start(ircd).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ircd_sup:start_link().

stop(_State) ->
    ok.
