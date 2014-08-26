#!/bin/bash

set -e

increase_limits() {
echo -n "User to increase limits for: "
read USER_TO_INCREASE

LIMITS_FILE=/etc/security/limits.conf

set +e
grep "$USER_TO_INCREASE"  "$LIMITS_FILE" > /dev/null
if [ "$?" == 0 ]; then
echo "ERROR: Looks like you've already tried to increase ${USER_TO_INCREASE}'s limits"
echo "Try editing $LIMITS_FILE directly."
exit 1
fi
set -e

cat <<EOF >> "$LIMITS_FILE"
${USER_TO_INCREASE} hard nofile 250000
${USER_TO_INCREASE} soft nofile 250000
EOF
}

RESULT=FAILURE
report_result() {
    echo $RESULT
}
trap "report_result;" EXIT
increase_limits
RESULT=OK

echo "OK: Limits increased. Log out and back in again for changes to take effect."
