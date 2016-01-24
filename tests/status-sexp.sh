# GNU Shepherd --- Test status sexps.
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

trap "rm -f $socket $conf $stamp $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

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
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
while ! test -f "$pid" ; do sleep 0.3 ; done

dmd_pid="`cat $pid`"

kill -0 $dmd_pid
test -S "$socket"

# Code to fetch service status info.
fetch_status="
  (let ((sock (open-connection \"$socket\")))
    (write-command (shepherd-command 'status 'root) sock)
    (read sock))"

root_service_sexp="
   (service (version 0)
      (provides (root shepherd))
      (requires ())
      (respawn? #f)
      (docstring \"The root service is used to operate on shepherd itself.\")
      (enabled? #t) (running #t) (last-respawns ()))"

"$GUILE" -c "
(use-modules (shepherd comm) (srfi srfi-1) (ice-9 match))

(exit
 (match $fetch_status
   (('reply _ ('result (services)) ('error #f) ('messages ()))
    (lset= equal?
            services
	   '($root_service_sexp
	     (service (version 0)
	       (provides (foo)) (requires ())
	       (respawn? #t) (docstring \"Foo!\")
	       (enabled? #t) (running 42)
	       (last-respawns ()))
	     (service (version 0)
	       (provides (bar)) (requires (foo))
	       (respawn? #f) (docstring \"Bar!\")
	       (enabled? #t) (running #f)
	       (last-respawns ())))))))
"

# Make sure we get an 'error' sexp when querying a nonexistent service.
"$GUILE" -c "
(use-modules (shepherd comm) (ice-9 match))

(match (let ((sock (open-connection \"$socket\")))
         (write-command (shepherd-command 'status 'does-not-exist) sock)
         (read sock))
  (('reply _ ...
    ('error ('error _ 'service-not-found 'does-not-exist))
    ('messages ()))
   #t)
  (x
   (pk 'wrong x)
   (exit 1)))"

# Unload everything and make sure only 'root' is left.
$herd unload root all

"$GUILE" -c "
(use-modules (shepherd comm))

(exit
  (equal? $fetch_status
          '(reply
            (version 0)
            (result (($root_service_sexp)))
            (error #f) (messages ()))))"

$herd stop root
! kill -0 $dmd_pid

test -f "$log"
