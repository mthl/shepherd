# GNU dmd --- Test basic communication capabilities.
# Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
pid="t-pid-$$"

deco="deco -s $socket"

trap "rm -f $socket $conf $stamp $log $pid;
      test -f $pid && kill \`cat $pid\` || true" EXIT

cat > "$conf"<<EOF
(use-modules (srfi srfi-26))
(register-services
 (make <service>
   #:provides '(test)
   #:start (lambda _
             (call-with-output-file "$stamp"
               (cut display "foo" <>))
             #t)
   #:stop  (lambda _
             (delete-file "$stamp"))
   #:respawn? #f))
EOF

rm -f "$pid"
dmd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do : ; done

dmd_pid="`cat $pid`"

kill -0 $dmd_pid
test -S "$socket"
$deco status dmd | grep -E '(Start.*dmd|Stop.*test)'

$deco start test
test -f "$stamp"
$deco status test | grep started

$deco stop test
! test -f "$stamp"

$deco status test | grep stopped

$deco stop dmd
! kill -0 $dmd_pid

test -f "$log"