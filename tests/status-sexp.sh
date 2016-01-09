# GNU dmd --- Test status sexps.
# Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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
pid="t-pid-$$"

deco="deco -s $socket"

trap "rm -f $socket $conf $stamp $log $pid;
      test -f $pid && kill \`cat $pid\` || true" EXIT

cat > "$conf"<<EOF
(register-services
 (make <service>
   #:provides '(foo)
   #:start (const 42)
   #:stop  (const #f)
   #:docstring "Foo!"
   #:respawn? #t)
 (make <service>
   #:provides '(bar)
   #:requires '(foo)
   #:start (const 'up-and-running)
   #:stop  (const #f)
   #:docstring "Bar!"
   #:respawn? #f))

(start 'foo)
EOF

rm -f "$pid"
dmd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do : ; done

dmd_pid="`cat $pid`"

kill -0 $dmd_pid
test -S "$socket"

# Code to fetch service status info.
fetch_status="
  (let ((sock (open-connection \"$socket\")))
    (write-command (dmd-command 'status 'dmd) sock)
    (read sock))"

dmd_service_sexp="
   (service (version 0)
      (provides (dmd)) (requires ())
      (respawn? #f)
      (docstring \"The dmd service is used to operate on dmd itself.\")
      (enabled? #t) (running #t) (last-respawns ()))"

"$GUILE" -c "
(use-modules (dmd comm) (srfi srfi-1))

(exit
 (lset= equal? $fetch_status
	       '(service-list (version 0)
                  $dmd_service_sexp
		  (service (version 0)
		     (provides (foo)) (requires ())
		     (respawn? #t) (docstring \"Foo!\")
		     (enabled? #t) (running 42)
		     (last-respawns ()))
		  (service (version 0)
		     (provides (bar)) (requires (foo))
		     (respawn? #f) (docstring \"Bar!\")
		     (enabled? #t) (running #f)
		     (last-respawns ())))))
"

# Unload everything and make sure only 'dmd' is left.
$deco unload dmd all

"$GUILE" -c "
(use-modules (dmd comm))

(exit
  (equal? $fetch_status
          '(service-list (version 0) $dmd_service_sexp)))"

$deco stop dmd
! kill -0 $dmd_pid

test -f "$log"
