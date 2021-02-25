#!/bin/bash
# NOTE: to restore, run the below rsync command in reverse, with sudo, changing
# -M--fake-super to --fake-super, ommitting --exclude and --link arguments

# config params
RUSER=dieggsy
RHOST=backup.dieggsy.com
RDIR=/mnt/wd-passport/backup
EXCLUDE=("/dev/*"
         "/proc/*"
         "/sys/*"
         "/tmp/*"
         "/run/*"
         "/mnt/*"
         "/media/*"
         "/lost+found"
         "/home/*/.local/share/Aspyr"
         "/home/*/.local/share/Steam"
         "/home/*/.local/share/Trash"
         "/home/*/.cache"
         "/home/*/.ccache")
BANDWIDTH=2M
OPTIONS=("-aAXx"
         "-M--fake-super"
         "--numeric-ids"
         "--partial-dir=.rsync-partial"
         "--bwlimit=$BANDWIDTH")

# necessary info
status=$2
UUID="$(hostname)-$(</etc/machine-id)"
DEST="$RUSER@$RHOST:$RDIR"
# Holds time info for last backup
backup_file="/var/cache/private/$UUID"
last_backup=$([[ -f $backup_file ]] && stat -c %Y $backup_file || echo 0)
currtime=$(date +%s)
one_day=86400

while getopts ":p" opt; do
    case $opt in
        p) show_progress="--info=progress2"
    esac
done

ionice -c 3 -p $$ &> /dev/null
renice +12 -p $$ &> /dev/null

# rsync process runnning
pgrep rsync &>/dev/null && exit 0

# Reasons to exit (irrelevant if -p is specified)
if [ -z "$show_progress" ]; then
    # status is something other than "up"
    [ "$status" != "up" ] && exit 0

    # last backup was successful, and less than a day has passed since
    # Error code 24 should be OK, it means the some source files vanished
    last_exit_code="$(<$backup_file)"
    [ $last_exit_code == 0 ] || [ $last_exit_code == 24 ] \
        && [ "$(($currtime - $last_backup))" -lt $one_day ] && exit 0
fi

# Run this in background, just in case. Networkmanager-dispather stops
# processes with a timeout, and although rsync might be forking on it's own...
# eh.
(TODAY=`date "+%Y-%m-%d"`
 LINKS=("$(date -d '-1 day' "+%Y-%m-%d")"
        "$(date -d '-2 day' "+%Y-%m-%d")"
        "$(date -d '-3 day' "+%Y-%m-%d")")
 cmd="rsync ${OPTIONS[@]} ${EXCLUDE[@]/#/--exclude=} ${LINKS[@]/#/--link-dest=../} / $DEST/$UUID/$TODAY"
 echo "BACKING UP: $cmd"
 ssh $RUSER@$RHOST "mkdir -p $RDIR/$UUID/"
 $cmd $show_progress
 echo $? > $backup_file) &
