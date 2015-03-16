#!/usr/bin/python

from __future__ import print_function

import subprocess as sp
import os
import os.path
import argparse
import shlex
import sys
import signal
import random


UNDAEMON_CGROUP_PATH = "/sys/fs/cgroup/undaemon"


class Undaemon(object):
    """
    WARNING: Since we can process only a set of signals, this class is not re-entrant...!!!
    """

    def __init__(self, cmd, user=None):
        self._cmd = cmd
        self._ready_to_finish = False
        self._cgroup_dir = None
        self._user = user


    @staticmethod
    def _random_name(length=10):
        name = ''.join(random.choice('abcdefghijklmnopqrstuvwxyz0123456789') for i in range(length))
        return name

    @staticmethod
    def _create_undaemon_fs_if_not_exists():
        if os.path.exists(UNDAEMON_CGROUP_PATH):
            return
        else:
            os.mkdir(UNDAEMON_CGROUP_PATH)
            sp.check_call(shlex.split("mount -t cgroup undaemon -ocpu {0}".format(UNDAEMON_CGROUP_PATH)))
            # Further setup the cgroup
            notify_on_release_flags = os.path.join(UNDAEMON_CGROUP_PATH, "notify_on_release")
            with open(notify_on_release_flags, "w") as out:
                print("1", file=out)
            release_agent_fname = os.path.join(UNDAEMON_CGROUP_PATH, "release_agent")
            this_filename = os.path.abspath(__file__)
            with open(release_agent_fname, "w") as out:
                print(this_filename, file=out)


    def _undameon_core(
            self,
            cgroup_subsystem,
            cgroup_name):

        cmd = self._cmd
        # "Create" the cgroup where the process will live...
        cgroup_dir = "/sys/fs/cgroup/{0}/{1}".format(cgroup_subsystem, cgroup_name)
        self._cgroup_dir = cgroup_dir
        os.makedirs(cgroup_dir)

        # After doing this, fork a task and add it to the newly created cgroup
        child_pid = os.fork()

        if child_pid == 0:
            # Drop privileges, if that's required.
            os.setuid(self._user)
            os.execvp(cmd[0], cmd)
        else:
            # I'm still the parent, add the child to the tasks on the process
            tasks_fname = os.path.join(cgroup_dir, "tasks")
            with open(tasks_fname, "a") as out:
                print(child_pid, file=out)


    def _set_signal_handlers(self):
        signal.signal(signal.SIGUSR1, self._usr1_handler)
        signal.signal(signal.SIGALRM, self._alarm_handler)
        signal.signal(signal.SIGTERM, self._kill_all)

    def _usr1_handler(self, sgn, frame):
        self._ready_to_finish = True
        os.rmdir(self._cgroup_dir)
        # Ensure that the process wakes up at lease once more
        os.kill(os.getpid(), signal.SIGALRM)

    def _alarm_handler(self, sgn, frame):
        pass

    def undameon(self):
        # ATTENTION: Since we can process only a set of signals, this class is not re-entrant...
        self._set_signal_handlers()
        self._create_undaemon_fs_if_not_exists()
        cgroup_name = str(os.getpid())
        self._undameon_core("undaemon", cgroup_name)
        # And wait until a signal is received
        while not self._ready_to_finish:
            signal.pause()
        # At due moment, just exit

    def _kill_all(self):
        # Very bloodily kill all processes in the cgroup
        tasks_fname = os.path.join(cgroup_dir, "tasks")
        with open(tasks_fname, "r") as inp:
            for line in inp:
                task_id = int(line)
                os.kill(task_id, signal.SIGTERM)

def main():
    argv = sys.argv
    try:
        child_cmd_start = argv[1:].index('--')
    except ValueError:
        main_for_release_agent(argv)
    else:
        cmd = argv[2+child_cmd_start:]
        argv1 = argv[:1+child_cmd_start]

        parser = argparse.ArgumentParser()
        parser.add_argument("--user", type=int, default=os.getuid())

        args = parser.parse_args(argv1[1:])


        u = Undaemon(cmd, args.user)
        u.undameon()


def main_for_release_agent(argv):
    cgroup_rel_path = argv[1]
    controller_pid = int(cgroup_rel_path[1:])
    os.kill(controller_pid, signal.SIGUSR1)


if __name__ == '__main__':
    main()

