#!/usr/bin/python

from __future__ import print_function

import subprocess as sp
import subprocess
import os
import os.path
import argparse
import shlex
import sys
import signal
import random
import time
import  logging


UNDAEMON_CGROUP_PATH = "/sys/fs/cgroup/undaemon"


logger = logging.getLogger("undaemon")


class Undaemon(object):
    """
    WARNING: Since we can process only a set of signals, this class is not re-entrant...!!!
    """

    def __init__(self, 
        cmd, 
        user=None,
        undaemon_cgroup_path = UNDAEMON_CGROUP_PATH
        ):
        self._cmd = cmd
        self._ready_to_finish = False
        self._cgroup_dir = None
        self._user = user
        self._undaemon_cgroup_path = undaemon_cgroup_path
        self._notify_exit = []

    @staticmethod
    def _random_name(length=10):
        name = ''.join(random.choice('abcdefghijklmnopqrstuvwxyz0123456789') for i in range(length))
        return name

    def _create_undaemon_fs_if_not_exists( self ):
        logger.info("Asserting privileges: ")
        if os.path.exists( self._undaemon_cgroup_path ):
            logger.info("(undaemon) Cgroup path %s already exists, trying to mount anyway", self._undaemon_cgroup_path)
            try:
                p = sp.Popen(
                    shlex.split("mount -t cgroup undaemon -ocpu {0}".format(self._undaemon_cgroup_path)),
                    stderr=sp.STDOUT,
                    stdout=sp.PIPE
                )
                complaint, none = p.communicate()
            except sp.CalledProcessError:
                logger.exception("(undaemon) remount went bad")
            else:
                if p.returncode != 0:
                    logger.error("(complaint) retcode=%d complaint: %s", p.returncode, complaint )
            return
        else:
            os.mkdir(self._undaemon_cgroup_path)
            sp.check_call(shlex.split("mount -t cgroup undaemon -ocpu {0}".format(self._undaemon_cgroup_path)))
            # Further setup the cgroup
            notify_on_release_flags = os.path.join(self._undaemon_cgroup_path, "notify_on_release")
            with open(notify_on_release_flags, "w") as out:
                print("1", file=out)
            release_agent_fname = os.path.join(self._undaemon_cgroup_path, "release_agent")
            this_filename = os.path.abspath(__file__)
            with open(release_agent_fname, "w") as out:
                print(this_filename, file=out)
            logger.info("(undaemon) Cgroup path %s just configured", self._undaemon_cgroup_path)

    def _undameon_core(
            self,
            # The cgroup_name is the pid of the controlling process....
            cgroup_name):

        cmd = self._cmd
        # "Create" the cgroup where the process will live...
        cgroup_dir = os.path.join(self._undaemon_cgroup_path, cgroup_name)
        logger.info("cgroup dir: %s", cgroup_dir)

        self._cgroup_dir = cgroup_dir
        
        try:
            os.makedirs(cgroup_dir)
        except os.error:
            # It means that there is a directory there already...
            # shouldn't be aproblem because only one process can have a given
            # PID, so either I'm a re-spawn of an unnaturally dead process, or 
            # this is a second invocation of undaemon for the same process...
            # In the second case, the release_agent doesn't do much, because 
            # I haven't found a simple way of having a controlling process to 
            # be notified of which particular cgroup exited (SIGUSR1 is a 
            # "monomorphic" signal in the sense that it can't carry a parameter
            # with the exited cgroup). 
            pass

        # After doing this, fork a task and add it to the newly created cgroup
        child_pid = os.fork()

        if child_pid == 0:
            # Drop privileges, if that's required.
            logger.info("(undaemon) I'm child: %d", os.getpid())
            os.setuid(self._user)
            os.execvp(cmd[0], cmd)
        else:
            logger.info("(undaemon) I'm controller: %d", os.getpid())
            self._root_pid = child_pid
            # I'm still the parent, add the child to the tasks on the process
            tasks_fname = os.path.join(cgroup_dir, "tasks")
            logger.info("tasks file: %s", tasks_fname)
            with open(tasks_fname, "a") as out:
                print(child_pid, file=out)

    def _set_signal_handlers(self):
        signal.signal(signal.SIGUSR1, self._usr1_handler)
        signal.signal(signal.SIGALRM, self._alarm_handler)
        signal.signal(signal.SIGTERM, self.kill_all)

    def _usr1_handler(self, sgn, frame):
        time.sleep(0.5)
        self._ready_to_finish = True
        os.rmdir(self._cgroup_dir)
        # Ensure that the process wakes up at lease once more
        os.kill(os.getpid(), signal.SIGALRM)

    def _alarm_handler(self, sgn, frame):
        pass

    def undaemon(self, set_signal_handlers = True):
        # ATTENTION: Since we can process only a set of signals, this class is not re-entrant...
        if set_signal_handlers :
            # This is optional, b ecause 
            self._set_signal_handlers()
        self._create_undaemon_fs_if_not_exists()
        cgroup_name = str(os.getpid())
        self._undameon_core(cgroup_name)
        # And wait until a signal is received
        while not self._ready_to_finish:
            signal.pause()
        # At due moment, just exit

    def kill_all(self, *args):
        # Very bloodily kill all processes in the cgroup
        cgroup_dir = self._cgroup_dir
        tasks_fname = os.path.join(cgroup_dir, "tasks")
        processes_killed = 0
        processes_terminated = 0
        just_in_case_list = []
        with open(tasks_fname, "r") as inp:
            for line in inp:
                task_id = int(line)
                just_in_case_list.append(task_id)
                logger.debug("Term %d", task_id)
                try:
                    os.kill(task_id, signal.SIGTERM)
                except OSError:
                    logger.error("attemping TERM")
                else:
                    processes_terminated += 1
        # Wait one second, maybe two ...
        time.sleep(2.0)
        # And go again...
        with open(tasks_fname, "r") as inp:
            for line in inp:
                task_id = int(line)
                logger.debug("Kill %d", task_id)
                try:
                    os.kill(task_id, signal.SIGKILL)
                except OSError:
                    logger.error("killing")
                else:
                    processes_killed += 1
                    processes_terminated -= 1
        # And a last time
        for task_id in just_in_case_list:
            try:
                os.kill(task_id, signal.SIGKILL)
            except OSError:
                pass

        return (processes_terminated, processes_killed)

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
        u.undaemon()


def main_for_release_agent(argv):
    cgroup_rel_path = argv[1]
    controller_pid = int(cgroup_rel_path[1:])
    os.kill(controller_pid, signal.SIGUSR1)


if __name__ == '__main__':
    main()

