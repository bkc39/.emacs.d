#!/bin/sh

EMD_DAEMON=$(which emacs)
EMD_PID_FILE='/var/run/emacs-daemon.pid'

if [ -z "$EMD_DAEMON" ]; then
    echo 'emacs not installed ... exiting'
    exit 0
fi

emd_start() {
    if [ ! -f "$EMD_PID_FILE" ]; then
        ## server is not running, run it and save the pid in a file
        touch "$EMD_PID_FILE" && emacs --daemon
        echo "$!" > "$EMD_PID_FILE"
    else
        echo 'Emacs server already running'
    fi
}


emd_stop() {
    if [ -f "$EMD_PID_FILE" ]; then
       emacsclient -e '(let ((last-nonmenu-event nil)) (save-buffers-kill-emacs))'
       rm -f "$EMD_PID_FILE"
    else
        echo 'emacs server not currently running'
    fi
}

emd_restart() {
    emd_stop
    emd_start
}

emd_start
