# GNU dmd --- Test respawnable services.
# Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of GNU dmd.
#
# GNU dmd is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU dmd is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU dmd.  If not, see <http://www.gnu.org/licenses/>.

dmd --version
deco --version

socket="t-socket-$$"
conf="t-conf-$$"
log="t-log-$$"
stamp="t-stamp-$$"
service_pid="t-service-pid-$$"

deco="deco -s $socket"
dmd_pid=""

trap "rm -f $socket $conf $stamp $log; test -z $dmd_pid || kill $dmd_pid; \
  test -f $service_pid && kill $service_pid" EXIT

function wait_for_file
{
    i=0
    while ! test -f "$1" && test $i -lt 20
    do
	cat "$log"
	sleep 0.3
	i=`expr $i + 1`
    done
    test -f "$1"
}

function assert_killed_service_is_respawned
{
    old_pid="`cat "$service_pid"`"
    rm "$service_pid"
    kill $old_pid

    wait_for_file "$service_pid"
    test -f "$service_pid"
    new_pid="`cat "$service_pid"`"

    grep -i "respawn.*test" "$log"
    : > "$log"

    test "$old_pid" -ne "$new_pid"
    kill -0 "$new_pid"
}

cat > "$conf"<<EOF
(register-services
 (make <service>
   #:provides '(test)
   #:start (make-forkexec-constructor
	    "$SHELL" "-c"
	    "echo \$\$ > $service_pid ; while true ; do : ; done")
   #:stop  (make-kill-destructor)
   #:respawn? #t))
(start 'test)
EOF

dmd -I -s "$socket" -c "$conf" -l "$log" &
dmd_pid=$!

sleep 1				# XXX: wait till it's up
kill -0 $dmd_pid
test -S "$socket"
$deco status test | grep started

test -f "$service_pid"
kill -0 `cat "$service_pid"`

# Now, kill the service, and make sure it gets respawned.
assert_killed_service_is_respawned
assert_killed_service_is_respawned
assert_killed_service_is_respawned

# Make sure the respawnable service can be stopped.
pid="`cat "$service_pid"`"
rm "$service_pid"
$deco stop test
$deco status test | grep stopped
! test -f "$service_pid"
! kill -0 "$pid"

$deco stop dmd
