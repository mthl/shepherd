# GNU dmd --- Test respawnable services.
# Copyright © 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
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
service1_pid="t-service1-pid-$$"
service2_pid="t-service2-pid-$$"
pid="t-pid-$$"

deco="deco -s $socket"

trap "cat $log || true ;
  rm -f $socket $conf $stamp $log $pid $service1_pid $service2_pid ;
  test -f $pid && kill \`cat $pid\` || true ;
  test -f $service1_pid && kill \`cat $service1_pid\` || true ;
  test -f $service2_pid && kill \`cat $service2_pid\` || true ;
  rm -f $service1_pid $service2_pid" EXIT

function wait_for_file
{
    i=0
    while ! test -f "$1" && test $i -lt 20
    do
	sleep 0.3
	i=`expr $i + 1`
    done
    test -f "$1"
}

function assert_killed_service_is_respawned
{
    old_pid="`cat "$1"`"
    rm "$1"
    kill $old_pid

    wait_for_file "$1"
    test -f "$1"
    new_pid="`cat "$1"`"

    test "$old_pid" -ne "$new_pid"
    kill -0 "$new_pid"
}

cat > "$conf"<<EOF
(register-services
 (make <service>
   #:provides '(test1)
   #:start (make-forkexec-constructor
	    '("$SHELL" "-c"
	      "echo \$\$ > $PWD/$service1_pid ; while true ; do sleep 1 ; done"))
   #:stop  (make-kill-destructor)
   #:respawn? #t)
 (make <service>
   #:provides '(test2)
   #:start (make-forkexec-constructor
	    '("$SHELL" "-c"
	      "echo \$\$ > $PWD/$service2_pid ; while true ; do sleep 1 ; done"))
   #:stop  (make-kill-destructor)
   #:respawn? #t))
(start 'test1)
(start 'test2)
EOF

rm -f "$pid"
dmd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
wait_for_file "$pid"

dmd_pid="`cat $pid`"

kill -0 $dmd_pid
test -S "$socket"
$deco status dmd
$deco status test1 | grep started
$deco status test2 | grep started

# The services are started, but that does not mean that they have
# written their PID file yet, so use 'wait_for_file' rather than
# 'test -f'.
wait_for_file "$service1_pid"
wait_for_file "$service2_pid"

# Make sure the PIDs are valid.
kill -0 `cat "$service1_pid"`
kill -0 `cat "$service2_pid"`

# Now, kill the services, and make sure they both get respawned.
assert_killed_service_is_respawned "$service1_pid"
assert_killed_service_is_respawned "$service2_pid"
assert_killed_service_is_respawned "$service1_pid"
assert_killed_service_is_respawned "$service2_pid"
assert_killed_service_is_respawned "$service1_pid"
assert_killed_service_is_respawned "$service2_pid"

# Make sure the respawnable service can be stopped.
pid="`cat "$service1_pid"`"
rm "$service1_pid"
$deco stop test1
$deco status test1 | grep stopped
! test -f "$service1_pid"
! kill -0 "$pid"

cat $service2_pid
$deco stop dmd
