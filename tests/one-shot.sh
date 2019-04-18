# GNU Shepherd --- Test one-shot services.
# Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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
   #:provides '(always-fail)
   #:start (const #f)
   #:one-shot? #t)
 (make <service>
   #:provides '(test)
   #:start (lambda _
             (call-with-output-file "$stamp"
               (cut display "foo" <>))
             #t)
   #:one-shot? #t)
 (make <service>
   #:provides '(test-2)
   #:requires '(test)
   #:start (lambda _
             (call-with-output-file "$stamp-2"
               (cut display "bar" <>))
             #t)
   #:stop  (lambda _
             (delete-file "$stamp-2")
             #f)))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

shepherd_pid="`cat $pid`"

kill -0 $shepherd_pid
test -S "$socket"

# Make sure we notice startup failures of one-shot services.
if $herd start always-fail; then false; else true; fi

for i in 1 2 3
do
    rm -f "$stamp"
    $herd start test
    test -f "$stamp"
    $herd status test | grep stopped
    grep "test.*started" "$log"
    $herd stop test		# no-op since it's already stopped
done

rm -f "$stamp" "$stamp-2"
$herd start test-2
test -f "$stamp"
test -f "$stamp-2"
$herd status test | grep stopped
$herd status test-2 | grep started
$herd stop test-2
if test -f "$stamp-2"; then false; else true; fi
