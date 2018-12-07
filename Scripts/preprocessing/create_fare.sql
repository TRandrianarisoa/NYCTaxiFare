-- Code to create the database
CREATE TABLE fare(
       id char(40),
       fare numeric(6, 2),
       date timestamp with time zone,
       ilong double precision,
       ilat double precision,
       olong double precision,
       olat double precision,
       pcount integer);
       


INSERT INTO fare VALUES('2009-06-15 17:26:21.0000001',4.5,'2009-06-15 17:26:21 UTC',-73.844311,40.721319,-73.84161,40.712278,1);

\copy fare FROM '10.csv.gz' WITH (FORMAT CSV);
       
