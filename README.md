# MPRIS Scrobbling Daemon
Daemon that scrobbles (ie monitors and registers music players' activity) by using DBus interfaces, for now all the data is stored locally in an SQLite database, support for services like LastFM and eventually other scrobbling services is planned.

# How to use
Before using it, you must create a config file called "scrobbling_config.cfg" in `~/.config/mpris_scrobblingd` (see Configuration section for more details on that). Then in the the directory of your choice, you need to create a db file called `scrobble.db` and execute the sql script in it. This is necessary in order to initialize it.

To run it, you just need to use `cabal run` in the project's directory, for now this is not actually a daemon since it doesn't start at boot but it can work as one if you start it manually, I'll work on that later.

Obviously this uses dbus so this is only usable on Linux.

# Configuration
The configuration file is pretty limited for now but it will be expanded in the future, here is an example one followed by the explanation of each line:
```
home_path=/home/user/scrobbler
time_to_register=0.5
log_level=debug
scrobbling_type=local
bus=org.mpris.MediaPlayer2.cmus:org.kde.elisa
```
* home_path: this is the directory where the database file is saved, note that haskell doesn't know ~ and that environment variables will not work there so you need the full path
* time_to_register: a float between 0 and 1 (both not included) that dictates how long to wait for registering a track as a proportion of the track's duration, in this case it's 0.5, meaning it will wait for the track to be at half duration before registering it
* log_level: does nothing for now
* scrobbling_type: does nothing for now, will be expanded when LastFM/LibreFM APIs will be implemented
* bus: colon separated list of bus names you want to listen to, you can use programs like d-spy or `busctl list` to find the bus you're loooking for