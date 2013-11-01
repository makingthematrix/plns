
# --- !Ups

DROP TABLE roottrans;
DROP TABLE roots;
DROP TABLE words;

CREATE TABLE dictentries(
	id serial primary key,
	fromword varchar(255) NOT NULL, 
	fromlang varchar(3) NOT NULL, 
    toword varchar(255) NOT NULL, 
    tolang varchar(3) NOT NULL, 
    caseid varchar(15) NOT NULL, 
    speechpart varchar(15) NOT NULL,
    speechpartid integer NOT NULL
);

CREATE TABLE uninflectedpairs (
	id serial primary key,
	fromword varchar(255) NOT NULL,
	toword varchar(255) NOT NULL
);

CREATE TABLE adverbpairs (
	id serial primary key,
	fromind varchar(255) NOT NULL,
	fromcmp varchar(255) NOT NULL,
	frommode varchar(15) NOT NULL,
	toind varchar(255) NOT NULL,
	tocmp varchar(255) NOT NULL, 
	cmpignored varchar(15) NOT NULL
);

CREATE TABLE adjectivepairs (
	id serial primary key,
	fromind varchar(255) NOT NULL, 
	fromadvind varchar(255) NOT NULL, 
	fromcmp varchar(255) NOT NULL, 
	fromadvcmp varchar(255) NOT NULL,
    frommode varchar(15) NOT NULL, 
    fromadvmode varchar(15) NOT NULL, 
    fromexceptions text,
    toind varchar(255) NOT NULL, 
    toadvind varchar(255) NOT NULL, 
    tocmp varchar(255) NOT NULL,
    toadvcmp varchar(255) NOT NULL,
    toexceptions text, 
    cmpignored varchar(15) NOT NULL
);

CREATE TABLE nounpairs (
	id serial primary key,
	fromstem varchar(255) NOT NULL, 
	frompattern varchar(255) NOT NULL, 
	fromexceptions text, 
    tostem varchar(255) NOT NULL, 
    topattern varchar(255) NOT NULL, 
    toexceptions text, 
    ignored varchar(15) NOT NULL
);

CREATE TABLE verbpairs (
	id serial primary key,
	frominfstem varchar(255) NOT NULL, 
	fromimpstem varchar(255) NOT NULL, 
	frompattern varchar(255) NOT NULL, 
	fromexceptions text,
	toinfstem varchar(255) NOT NULL, 
	toimpstem varchar(255) NOT NULL, 
	topattern varchar(255) NOT NULL, 
	toexceptions text,
	prefixes text
);


# --- !Downs

DROP TABLE dictentries;
DROP TABLE verbpairs;
DROP TABLE nounpairs;
DROP TABLE adjectivepairs;
DROP TABLE adverbpairs;
DROP TABLE uninflectedpairs;

CREATE TABLE words (
    id serial primary key,
    word varchar(255),
    lang varchar(3),
    rootid integer NOT NULL,
    caseid varchar(15)
);

CREATE TABLE roots (
	id serial primary key,
	root varchar(255),
	speechpart varchar(15),
	lang varchar(3)
);

CREATE TABLE roottrans (
	id serial primary key,
	rootid1 integer NOT NULL,
	rootid2 integer NOT NULL
);

