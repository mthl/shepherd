# GNU Shepherd --- Test the #:pid-file option of 'make-forkexec-constructor'.
# Copyright © 2016, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
  '("$SHELL" "-c" "echo \$\$ > $PWD/$service_pid ; exec sleep 600"))

(define %daemon-command
  ;; Emulate a daemon by forking and exiting right away.
  (quasiquote ("guile" "-c"
    ,(object->string '(when (zero? (primitive-fork))
                        (call-with-output-file "$PWD/$service_pid"
                          (lambda (port)
                            (display (getpid) port)))
                        (sleep 100))))))

(define %daemon-command-successful
  ;; Purposefully introduce a delay between the time the PID file
  ;; is created and the time it actually contains a valid PID.  This
  ;; simulates PID files not created atomically, as is the case with
  ;; wpa_supplicant 2.7 for instance.
  (quasiquote ("guile" "-c"
    ,(object->string '(when (zero? (primitive-fork))
                        (call-with-output-file "$PWD/$service_pid"
                          (lambda (port)
                            (usleep 1500000)
                            (display (getpid) port)))
                        (sleep 100))))))

(register-services
 (make <service>
   ;; A service that never produces its PID file, yet leaves a process
   ;; behind it.
   #:provides '(test)
   #:start (make-forkexec-constructor %command
                                      #:pid-file "/does-not-exist"

                                      ;; Low-end ARMv7 machines are
                                      ;; slow enough that creating
                                      ;; $service_pid could take
                                      ;; up to 4 seconds or so.
                                      #:pid-file-timeout 6)
   #:stop  (make-kill-destructor)
   #:respawn? #f)

 (make <service>
   ;; Same one, but actually produces the PID file.
   #:provides '(test-works)
   #:start (make-forkexec-constructor %daemon-command-successful
                                      #:pid-file "$PWD/$service_pid"
                                      #:pid-file-timeout 6)
   #:stop  (make-kill-destructor)
   #:respawn? #f)

 (make <service>
   ;; This one "daemonizes", fails to create a PID file, but leaves
   ;; a child process behind it.
   #:provides '(test-daemonizes)
   #:start (make-forkexec-constructor %daemon-command
                                      #:pid-file "/does-not-exist"
                                      #:pid-file-timeout 6)
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

# Now a service that "daemonizes" but fails to start.
rm -f "$service_pid"
if $herd start test-daemonizes
then false; else true; fi

$herd status test-daemonizes | grep stopped

# Make sure it did not leave its child process behind it.
test -f "$service_pid"
if kill -0 `cat "$service_pid"`
then false; else true; fi

# Now start the service that works.
$herd start test-works
$herd status test-works | grep started
test -f "$service_pid"
kill -0 "`cat $service_pid`"
known_pid="`$herd status test-works | grep Running \
   | sed -es'/.*Running value.* \([0-9]\+\)\.$/\1/g'`"
test `cat $service_pid` -eq $known_pid

$herd stop test-works
if kill -0 `cat "$service_pid"`
then false; else true; fi
