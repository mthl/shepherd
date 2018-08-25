# GNU Shepherd --- Test restarting services.
# Copyright © 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
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
log="t-log-$$"
pid="t-pid-$$"

herd="herd -s $socket"

trap "cat $log || true ;
  rm -f $socket $conf $log;
  test -f $pid && kill \`cat $pid\` || true ; rm -f $pid" EXIT

cat > "$conf"<<EOF
(register-services
 (make <service>
   #:provides '(test1)
   #:start (const #t)
   #:stop  (const #t))
 (make <service>
   #:provides '(test2)
   #:requires '(test1)
   #:start (const #t)
   #:stop  (const #t))
 (make <service>
   #:provides '(test3)
   #:requires '(test2)
   #:start (const #t)
   #:stop  (const #t)))
EOF

rm -f "$pid"
shepherd -I -s "$socket" -c "$conf" -l "$log" --pid="$pid" &

while ! test -f "$pid" ; do sleep 0.3 ; done

# Start some test services, and make sure they behave how we expect
$herd start test1
$herd start test2
$herd status test1 | grep started
$herd status test2 | grep started

# Restart test1 and make sure that both services are still running (ie. that
# test2 hasn't been stopped)
$herd restart test1
$herd status test1 | grep started
$herd status test2 | grep started

# Now let's test with a transitive dependency
$herd start test3
$herd status test3 | grep started

# After restarting test1 we want test3 to still be running
$herd restart test1
$herd status test1 | grep started
$herd status test2 | grep started
$herd status test3 | grep started
