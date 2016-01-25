# GNU Shepherd --- Test respawnable services.
# Copyright © 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
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
herd --version

socket="t-socket-$$"
conf="t-conf-$$"
log="t-log-$$"
stamp="t-stamp-$$"
service1_pid="t-service1-pid-$$"
service2_pid="t-service2-pid-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "cat $log || true ;
  rm -f $socket $conf $stamp $log ;
  test -f $pid && kill \`cat $pid\` || true ; rm -f $pid ;
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
            ;; The 'sleep' below is just to make it more likely
            ;; that synchronization issues in handling #:pid-file
            ;; would be caught.
	    '("$SHELL" "-c"
	      "sleep 0.7 ; echo \$\$ > $PWD/$service2_pid ; while true ; do sleep 1 ; done")
            #:pid-file "$PWD/$service2_pid")
   #:stop  (make-kill-destructor)
   #:respawn? #t))
(start 'test1)
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
wait_for_file "$pid"

shepherd_pid="`cat $pid`"

kill -0 $shepherd_pid
test -S "$socket"
$herd status
$herd status test1 | grep started

$herd start test2
$herd status test2 | grep started

# When 'herd start test2' returns, the PID file must already be created.
test -f "$service2_pid"

# Conversely, 'test1' may not have written its PID file yet, so use
# 'wait_for_file' rather than 'test -f'.
wait_for_file "$service1_pid"

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
$herd stop test1
$herd status test1 | grep stopped
! test -f "$service1_pid"
! kill -0 "$pid"

cat $service2_pid
$herd stop root
