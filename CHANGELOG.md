# Revision history for mpris-scrobbling-daemon

## 0.1.0.0 -- 19-12-2023

* First version. Released on an unsuspecting world.

## 0.1.1 -- 03-01-2024
* Made the player switching part work
* creating the JSON files isn't necessary anymore
* fixed stats not updating properly
* added music players used to 

## 0.1.2 -- 04-08-2024
* removed Stats and JsonOperations modules
* simplified architecture to use a simpole SQLite db instead of a patchwork of json files and db
* refactored code to account for these changes