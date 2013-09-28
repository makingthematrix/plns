 
# --- !Ups

CREATE TABLE roottrans (
	id serial primary key,
	rootid1 integer NOT NULL,
	rootid2 integer NOT NULL
);

# --- !Downs

DROP TABLE roottrans;