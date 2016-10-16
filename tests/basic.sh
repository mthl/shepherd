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

trap "cat $log || true; rm -f $socket $conf $stamp $log;
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
   #:respawn? #f)
 (make <service>
   #:provides '(broken)
   #:requires '()
   #:start (lambda _
             (mkdir "/this/throws/a/system/error"))
   #:stop  (const #f)
   #:respawn? #f))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

kill -0 $shepherd_pid
test -S "$socket"
pristine_status=`$herd status root` # Prep for 'reload' test.
echo $pristine_status | grep -E '(Start.*root|Stop.*test)'

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

# This used to crash shepherd: <http://bugs.gnu.org/24684>.
if $herd enable test-2 with extra arguments
then false; else true; fi

$herd status test-2 | grep started

# Make sure extra arguments lead to an error.
if $herd status test-2 something else that is useless
then false; else true; fi

for action in status start stop
do
    if $herd $action does-not-exist
    then false; else true; fi

    $herd $action does-not-exist 2>&1 | grep "does-not-exist.*not.*found"
done

if $herd an-action-that-does-not-exist root
then false; else true; fi

if $herd start broken
then false; else true; fi

# Wrong number of arguments for an action.
if $herd status root foo bar baz;
then false; else true; fi

# Asking for the doc of specific actions.
$herd doc root action status
if $herd doc root action an-action-that-does-not-exist
then false; else true; fi

# Make sure the error message is correct.
$herd doc root action an-action-that-does-not-exist 2>&1 | \
    grep "does not have an action 'an-action-that-does-not-exist'"

# Loading nonexistent file.
if $herd load root /does/not/exist.scm;
then false; else true; fi

# Unload two services, make sure the other it still around.
$herd unload root broken
$herd unload root test
$herd status | grep "Stopped: (test-2)"

$herd reload root "$conf"
test "`$herd status`" == "$pristine_status"

# Dynamically loading code.

mkdir -p "$confdir"
cat > "$confdir/some-conf.scm" <<EOF
(register-services
 (make <service>
   #:provides '(test-loaded)
   #:start (const 42)
   #:stop (const #f)))
EOF

if $herd status test-loaded
then false; else true; fi

# Pass a relative file name and makes sure it's properly resolved.
(cd "$confdir" && herd -s "../$socket" load root "some-conf.scm")
rm "$confdir/some-conf.scm"

# The new service should be loaded now.
$herd status test-loaded
$herd status test-loaded | grep stopped

$herd start test-loaded
$herd status test-loaded | grep -i 'running.*42'
$herd stop test-loaded
$herd unload root test-loaded

# Load a service whose running value does not have a valid read syntax, and
# make sure that the running value is clamped before being sent over the wire.
cat > "$confdir/some-conf.scm" <<EOF
(register-services
 (make <service>
   #:provides '(test-loaded)
   #:start (const (if #f #f))  ;#<undefined>
   #:stop (const #f)))
EOF

$herd load root "$confdir/some-conf.scm"
$herd start test-loaded
$herd status test-loaded | grep -i "running.*#<unspecified>"
$herd stop test-loaded

# Deregister 'test-loaded' via 'eval'.
$herd eval root "(action root-service 'unload \"test-loaded\")"
if $herd status test-loaded
then false; else true; fi

# Evaluate silly code, make sure nothing breaks.
if $herd eval root '(/ 0 0)'
then false; else true; fi

if $herd eval root '(no closing paren'
then false; else true; fi

# Unload everything and make sure only 'root' is left.
$herd unload root all
$herd status | grep "Stopped: ()"
$herd status | grep "Started: (root)"

$herd stop root
! kill -0 $shepherd_pid

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

shepherd_pid="`cat $pid`"

$herd stop root
! kill -0 $shepherd_pid

rm -rf $confdir
