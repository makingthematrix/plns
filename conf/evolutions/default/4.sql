# --- !Ups

CREATE TABLE roots (
	id serial primary key,
	root varchar(255),
	speechpart varchar(15),
	lang varchar(3),
	speechpartid integer not null
);

# --- !Downs
 
DROP TABLE roots;