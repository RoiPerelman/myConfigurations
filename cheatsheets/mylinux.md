Roi's bashsheet
===============

## Processes
* `top` - display and **update** sorted information about processes
* `ps` - process status (like top only a single snapshot)
  * `-e` - Display information about other users' processes as well as your own
  * `-f` -  Display the uid, pid, parent pid, recent CPU usage, process start time, controlling tty, elapsed CPU usage, and the associated command.
* `df -H` - display free disk space with "Human-readable" output
* `lsof -i ${port}` - list open files with whose internet address corresponds with port
* `kill -KILL ${pid}` / `kill -9 ${pid}` - SIGKILL the pid (force quit a process)

## Other
* `printenv` - get all environmental variables
* `ssh-add ~/.ssh/${id_password}` - adds private key identities to the authentication agent
* `which` / `whereis` - locate a program file in the user's path / locate programs
* `date +'%Y %m %d %M'` - give UTC date (Israel IDT is +3 hours from UTC)