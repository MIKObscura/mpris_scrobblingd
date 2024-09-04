CREATE TABLE "scrobbles" (
	"id"	INTEGER NOT NULL UNIQUE,
	"timestamp"	INTEGER NOT NULL,
	"player" TEXT NOT NULL,
	"artist" TEXT NOT NULL,
	"album" TEXT NOT NULL,
	"title" TEXT NOT NULL,
	"duration" INTEGER NOT NULL,
	PRIMARY KEY("id" AUTOINCREMENT)
);