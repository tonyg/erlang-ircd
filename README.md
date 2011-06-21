# erlang-ircd

`erlang-ircd` is a pluggable Erlang IRC daemon application or
library. The user supplies a callback module which the daemon uses to
create and delete rooms, route messages etc.

The code is embryonic at the moment: a stub callback module is
implemented that uses a toy process-per-room implementation of shared
rooms. It would also be interesting to have a callback module for
routing messages into and out of RabbitMQ.

## Running the code

Type `make run`. If all goes well, you will have an IRC server
listening on `localhost`'s port 6667. Connect using an IRC client. For
example, using [irssi](http://irssi.org/):

    irssi -c localhost

Try `/join`, `/part` and speaking to see what the demo callback module
does.

## License

Copyright (C) 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>

`erlang-ircd` is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.

`erlang-ircd` is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with `erlang-ircd`.  If not, see <http://www.gnu.org/licenses/>.
