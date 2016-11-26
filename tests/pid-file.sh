# GNU Shepherd --- Test the #:pid-file option of 'make-forkexec-constructor'.
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
service_pid="t-service-pid-$$"

herd="herd -s $socket"

trap "cat $log || true; rm -f $socket $conf $service_pid $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf"<<EOF
(use-modules (ice-9 match))

(define %command
  '("$SHELL" "-c" "echo \$\$ > $PWD/$service_pid ; sleep 600"))

(register-services
 (make <service>
   ;; A service that never produces its PID file, yet leaves a process
   ;; behind it.
   #:provides '(test)
   #:start (make-forkexec-constructor %command
                                      #:pid-file "/does-not-exist"
                                      #:pid-file-timeout 2)
   #:stop  (make-kill-destructor)
   #:respawn? #f))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

# The service is expected to fail to start.
if $herd start test
then false; else true; fi

# Make sure it is marked as stopped.
$herd status test | grep stopped

test -f "$service_pid"

# Make sure it did not leave a process behind it.
if kill -0 `cat "$service_pid"`
then false; else true; fi
