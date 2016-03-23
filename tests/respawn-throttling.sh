# GNU Shepherd --- Test respawn throttling.
# Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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
pid="t-pid-$$"

herd="herd -s $socket"

trap "cat $log || true ;
  rm -f $socket $conf $log ;
  test -f $pid && kill \`cat $pid\` || true ; rm -f $pid" EXIT

cat > "$conf"<<EOF
(register-services
 (make <service>
   #:provides '(keeps-respawning)
   #:start (make-forkexec-constructor '("false"))
   #:stop  (make-kill-destructor)
   #:respawn? #t))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done
shepherd_pid="`cat $pid`"

kill -0 $shepherd_pid
test -S "$socket"

$herd start keeps-respawning

# Maximum number of seconds to wait.  XXX: It takes a while because SIGCHLD
# handling is deferred until we leave the accept(2) call in (shepherd).
count=15

while [ $count -gt 0 ]
do
    sleep 1
    if $herd status keeps-respawning | grep disabled
    then
	# The service is now disabled: success!
	break
    else
	count=`expr $count - 1`
	test $count -ge 0
    fi
done

# Make sure the service is indeed stopped and disabled.
$herd status keeps-respawning | grep stopped
$herd status keeps-respawning | grep disabled
if $herd start keeps-respawning
then false; else true; fi

$herd status keeps-respawning | grep -i "last respawned"

grep -i "respawning too fast" "$log"
