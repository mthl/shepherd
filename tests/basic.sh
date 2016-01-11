# GNU Shepherd --- Test basic communication capabilities.
# Copyright © 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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
   #:respawn? #f)
 (make <service>
   #:provides '(test-2)
   #:requires '(test)
   #:start (lambda _
             (call-with-output-file "$stamp-2"
               (cut display "bar" <>))
             #t)
   #:stop  (lambda _
             (delete-file "$stamp-2"))
   #:respawn? #f))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do : ; done

dmd_pid="`cat $pid`"

kill -0 $dmd_pid
test -S "$socket"
pristine_status=`$deco status dmd` # Prep for 'reload' test.
echo $pristine_status | grep -E '(Start.*dmd|Stop.*test)'

$deco start test
test -f "$stamp"
$deco status test | grep started

$deco stop test
! test -f "$stamp"

$deco status test | grep stopped

$deco start test-2

$deco status test-2 | grep started

# Unload one service, make sure the other it still around.
$deco unload dmd test
$deco status | grep "Stopped: (test-2)"

$deco reload dmd "$conf"
test "`$deco status`" == "$pristine_status"

# Unload everything and make sure only 'dmd' is left.
$deco unload dmd all
$deco status | grep "Stopped: ()"
$deco status | grep "Started: (dmd)"

$deco stop dmd
! kill -0 $dmd_pid

test -f "$log"
