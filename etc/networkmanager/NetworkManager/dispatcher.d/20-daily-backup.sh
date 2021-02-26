#!/bin/bash
exit 0

EXCLUDE=("*.o"
         "*.so"
         "/home/dieggsy/.local"
         "/home/dieggsy/.cache"
         "/home/dieggsy/.ccache"
         "/home/dieggsy/.rustup"
         "/home/dieggsy/.cargo"
         "/home/dieggsy/.minecraft"
         "/home/dieggsy/.config/Signal/attachments.noindex"
         "/home/dieggsy/.config/Artix*"
         "/home/dieggsy/.AndroidStudio*"
         "/home/dieggsy/dotfiles/emacs/.emacs.d/straight"
         "/home/dieggsy/src/project-euler/rust/target"
         "/home/dieggsy/pic/wallpapers/nasa-visions/originals"
         "/home/dieggsy/music"
         "/home/dieggsy/downloads")
# BANDWIDTH=50000

# necessary info
status=$2
# Holds time info for last backup
backup_file="/var/cache/private/dieggsy-$(</etc/machine-id)"
test -f $backup_file || touch $backup_file
archive="/var/cache/private/snapshot.tar.zst"
last_backup=$([[ -f $backup_file ]] && stat -c %Y $backup_file || echo 0)
currtime=$(date +%s)
one_day=86400

ionice -c 3 -p $$ &> /dev/null
renice +12 -p $$ &> /dev/null

# tar process runnning
pgrep -f 'tar --zstd' &>/dev/null && exit 0

# Reasons to exit (irrelevant if -p is specified)
# status is something other than "up"
[ "$status" != "up" ] && exit 0

# last backup was successful, and less than a day has passed since
# Error code 24 should be OK, it means the some source files vanished
last_exit_code="$(<$backup_file)"
[[ $last_exit_code == 0 ]] \
    && [[ "$(($currtime - $last_backup))" -lt $one_day ]] \
    && exit 0

# Run this in background, just in case. Networkmanager-dispatcher stops
# processes with a timeout, and although rsync might be forking on it's own...
# eh.
(backup="tar --zstd ${EXCLUDE[@]/#/--exclude=} -cf $archive /home/dieggsy"
 copy="scp $archive dieggsy@dieggsy.com:~/"
 echo BACKING UP
 $backup && $copy
 status=$?
 echo $status > $backup_file
 echo FINISHED WITH STATUS $status) &
