# MPRIS Scrobbling Daemon
This is a remake of an old project of mine, it was a scrobbler for the music player [Cmus](https://cmus.github.io/) that saved all your data locally in an SQLite database and optionnally in JSON files too. This project has the same idea but instead uses dbus to listen to whatever music player is active.

This program is in a very early stage, it will take some time before being actually practical to use.
I chose Haskell for this project for no particular reason, I was just learning the language when I got that idea so I went with it.

# How to use
Before using it, you must create a config file called "scrobbling_config.cfg" in `~/.config/mpris_scrobblingd` (see Configuration section for more details on that). Then in the the directory of your choice, you need to create a db file called `scrobble.db` and execute the sql script in it. This is necessary in order to initialize it. Then you also need to create an empty JSON file called `stats.json` and some JSON files with only "[]" as content called `scrobble_weekly_data.json`, `scrobble_monthly_data.json`, `scrobble_yearly_data.json` (I know this is kind of annoying, I'll work on making this not necessary).

To run it, you just need to use `cabal run` in the project's directory, for now this is not actually a daemon since it doesn't start at boot but it can work as one if you start it manually, I'll work on that later.

Obviously this uses dbus so this is only usable on Linux.

# Configuration
The configuration file is pretty limited for now but it will be expanded in the future, here is an example one followed by the explanation of each line:
```
home_path=/home/user/scrobbler
weekly_stats=true
monthly_stats=true
yearly_stats=true
keep_previous_sessions=true
time_to_register=0.5
log_level=debug
scrobbling_type=local
bus=org.mpris.MediaPlayer2.cmus:org.kde.elisa
```
* home_path: this is the directory where the database file as well as all of the JSON files are saved, note that haskell doesn't know ~ and that environment variables will not work there so you need the full path
* weekly_stats: true if you want to save weekly stats, false if not
* monthly_stats: true if you want to save monthly stats, false if not
* yearly_stats: true if you want to save yearly stats, false if not
* keep_previous_session: true if you want to keep every session in a different JSON file, if false, only the last session is saved
* time_to_register: a float between 0 and 1 (both not included) that dictates how long to wait for registering a track as a proportion of the track's duration, in this case it's 0.5, meaning it will wait for the track to be at half duration before registering it
* log_level: does nothing for now
* scrobbling_type: does nothing for now, will be expanded when LastFM/LibreFM APIs will be implemented
* bus: colon separated list of bus names you want to listen to, you can use programs like d-spy or `busctl list` to find the bus you're loooking for