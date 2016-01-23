# GNU Shepherd --- Test basic communication capabilities.
# Copyright © 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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
herd --version

socket="t-socket-$$"
conf="t-conf-$$"
confdir="t-confdir-$$"
log="t-log-$$"
stamp="t-stamp-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "rm -f $socket $conf $stamp $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

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
while ! test -f "$pid" ; do sleep 0.3 ; done

dmd_pid="`cat $pid`"

kill -0 $dmd_pid
test -S "$socket"
pristine_status=`$herd status dmd` # Prep for 'reload' test.
echo $pristine_status | grep -E '(Start.*dmd|Stop.*test)'

$herd start test
test -f "$stamp"
$herd status test | grep started

$herd stop test
! test -f "$stamp"

$herd status test | grep stopped

# Disable a service and make sure it cannot be started.
$herd disable test-2
if $herd start test-2
then false; else true; fi

$herd enable test-2
$herd start test-2

$herd status test-2 | grep started

for action in status start stop
do
    if $herd $action does-not-exist
    then false; else true; fi

    $herd $action does-not-exist 2>&1 | grep "does-not-exist.*not.*found"
done

if $herd an-action-that-does-not-exist dmd
then false; else true; fi

# Wrong number of arguments for an action.
if $herd status dmd foo bar baz;
then false; else true; fi

# Asking for the doc of specific actions.
$herd doc dmd action status
if $herd doc dmd action an-action-that-does-not-exist
then false; else true; fi

# Loading nonexistent file.
if $herd load dmd /does/not/exist.scm;
then false; else true; fi

# Unload one service, make sure the other it still around.
$herd unload dmd test
$herd status | grep "Stopped: (test-2)"

$herd reload dmd "$conf"
test "`$herd status`" == "$pristine_status"

# Unload everything and make sure only 'dmd' is left.
$herd unload dmd all
$herd status | grep "Stopped: ()"
$herd status | grep "Started: (dmd)"

$herd stop dmd
! kill -0 $dmd_pid

test -f "$log"

## ------------------------ ##
## Usage of XDG variables.  ##
## ------------------------ ##

# Set XDG_CONFIG_HOME for configuration files.
export XDG_CONFIG_HOME=$confdir
mkdir -p $confdir/shepherd
mv $conf $confdir/shepherd/init.scm
rm -f "$pid"
shepherd -I -s "$socket" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

# Launch a service from $confdir/shepherd/init.scm.
$herd start test
test -f "$stamp"
$herd status test | grep started

$herd stop test
! test -f "$stamp"

dmd_pid="`cat $pid`"

$herd stop dmd
! kill -0 $dmd_pid

rm -rf $confdir
