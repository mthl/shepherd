# GNU Shepherd --- Make sure shepherd doesn't fail when $HOME is not writable.
# Copyright © 2014, 2016 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of the GNU Shepherd.
#
# The GNU Shepherd is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# The GNU Shepherd is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.

shepherd --version

HOME=/nowhere
export HOME

if test -w "$HOME"
then
    # We cannot run this test.
    exit 77
fi

socket="t-socket-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "rm -f $socket $pid;
      test -f $pid && kill \`cat $pid\` || true" EXIT

# Make sure 'dmd' starts even though $HOME is not writable.
shepherd -I -s "$socket" -c /dev/null -l /dev/null --pid="$pid" &
dmd_pid="$!"

# Wait until it's ready, or until it terminates.
while ! test -f "$pid" ; do kill -0 "$dmd_pid" ; sleep 0.3 ; done

kill -0 `cat "$pid"`
$herd status dmd
$herd stop dmd

if kill `cat "$pid"`
then
    exit 1
fi
