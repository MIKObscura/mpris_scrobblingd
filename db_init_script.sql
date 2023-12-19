CREATE TABLE "artists" (
	"id"	INTEGER NOT NULL UNIQUE,
	"name"	TEXT NOT NULL UNIQUE,
	PRIMARY KEY("id" AUTOINCREMENT)
);
CREATE TABLE "albums" (
	"id"	INTEGER NOT NULL UNIQUE,
	"title"	TEXT NOT NULL,
	"artist"	INTEGER NOT NULL,
	FOREIGN KEY("artist") REFERENCES "artists"("id"),
	PRIMARY KEY("id" AUTOINCREMENT)
);
CREATE TABLE "tracks" (
	"id"	INTEGER NOT NULL UNIQUE,
	"title"	TEXT NOT NULL,
	"artist"	INTEGER NOT NULL,
	"album"	INTEGER NOT NULL,
	"plays"	INTEGER DEFAULT 0,
	"duration"	INTEGER,
	FOREIGN KEY("album") REFERENCES "albums"("id"),
	FOREIGN KEY("artist") REFERENCES "artists"("id"),
	PRIMARY KEY("id" AUTOINCREMENT)
);
create trigger increment_if_exist
before insert on tracks
when exists (select * from tracks where artist = new.artist and album = new.album and title = new.title and duration = new.duration)
begin
	update tracks
	set plays = plays + 1
	where artist = new.artist and album = new.album and title = new.title and duration = new.duration;
	select raise(ignore);
end;
create trigger create_album_if_not_exist
before insert on albums
when exists (select * from albums where title = new.title and artist = new.artist)
begin
	select raise(ignore);
end;
create trigger create_artist_if_not_exist
before insert on artists
when exists (select * from artists where name = new.name)
begin
	select raise(ignore);
end;