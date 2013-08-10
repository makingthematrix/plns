# Tasks schema
 
# --- !Ups

CREATE TABLE roots (
	id serial primary key,
	root varchar(255),
	speechpart varchar(15),
	lang varchar(3)
);

CREATE TABLE words (
    id serial primary key,
    word varchar(255),
    lang varchar(3),
    rootid integer NOT NULL,
    caseid varchar(15)
);

CREATE TABLE translations (
	id serial primary key,
	wordid1 integer NOT NULL,
	wordid2 integer NOT NULL
);
 
CREATE TABLE roottrans (
	id serial primary key,
	rootid1 integer NOT NULL,
	rootid2 integer NOT NULL
);
 
# --- !Downs
 
DROP TABLE roots;
DROP TABLE words;
DROP TABLE translations;