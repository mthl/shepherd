# GNU Shepherd --- Ensure replacing services works properly
# Copyright © 2014, 2016 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
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
rconf="t-rconf-$$"
log="t-log-$$"
stamp="t-stamp-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "rm -f $socket $conf $rconf $stamp $log;
      test -f $pid && kill \`cat $pid\` || true; rm -f $pid" EXIT

cat > "$conf"<<EOF
(use-modules (srfi srfi-26))
(register-services
 (make <service>
   #:provides '(test)
   #:start (const #t)
   #:actions (make-actions
              (say-hello (lambda _
                          (call-with-output-file "$stamp"
                           (lambda (port)
                            (display "Hello" port))))))
   #:respawn? #f))
EOF

rm -f "$pid" "$stamp" "$socket"
shepherd -I -s "$socket" -c "$conf" --pid="$pid" --log="$log" &

while ! test -f "$pid"; do sleep 0.5 ; done

$herd start test

if ! $herd say-hello test; then
    echo "say-hello failed"
    exit 1
fi

cat > "$rconf"<<EOF
(register-services
 (make <service>
   #:provides '(test)
   #:start (const #t)
   #:actions (make-actions
              (say-goodbye (lambda _
                             (call-with-output-file "$stamp"
                              (lambda (port)
                                (display "Goodbye" port))))))
   #:respawn? #f))
EOF

$herd load root "$rconf"

if ! $herd say-hello test; then
    echo "say-hello failed after setting replacement"
    exit 1
fi

if test "`cat $stamp`" != "Hello"; then
    echo "Output file had the wrong contents! Was:"
    cat $stamp
    exit 1
fi

$herd stop test

$herd start test

if $herd say-hello test; then
    echo "say-hello should have failed after stop/start"
    exit 1
fi

if ! $herd say-goodbye test; then
    echo "say-goodbye failed after replacement"
    exit 1
fi

if test "`cat $stamp`" != "Goodbye"; then
    echo "Output file had the wrong contents! Was:"
    cat $stamp
    exit 1
fi
