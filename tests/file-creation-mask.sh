# GNU Shepherd --- Test the #:file-creation-mask option of 'make-forkexec-constructor'.
# Copyright © 2020 Diego N. Barbato <dnbarbato@posteo.de>
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
service_log="t-service-log-$$"
service_new_file="t-service-new-file-$$"

herd="herd -s $socket"

trap "cat $log || true;
      rm -f $socket $conf $log $service_log $service_new_file;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

function wait_for_file
{
    i=0
    while ! test -f "$1" && test $i -lt 20
    do
	sleep 0.3
	i=`expr $i + 1`
    done
    test -f "$1"
}

cat > "$conf"<<EOF
(define %command
  '("$SHELL" "-c" "touch $PWD/$service_new_file; echo foo"))

(register-services
 (make <service>
   #:provides '(test)
   #:start (make-forkexec-constructor %command
                                      #:log-file "$PWD/$service_log"
                                      ;; Set the umask such that file
                                      ;; permissions are #o600.
                                      #:file-creation-mask #o177)
   #:stop (make-kill-destructor)
   #:respawn? #f))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

# Wait till it's ready.
wait_for_file "$pid"

# Start the service.
$herd start test

# Make sure the log file is created with the right permissions independently
# of the value of #:file-creation-mask.
wait_for_file "$service_log"
test `stat -c %a "$service_log"` -eq 640

# Make sure the service creates files with the right permissions as determined
# by the value of #:file-creation-mask.
wait_for_file "$service_new_file"
test `stat -c %a "$service_new_file"` -eq 600
