-- A minimal set of test tables for databases that don't support PRIMARY KEY.

CREATE TABLE fooble(id INTEGER, comment TEXT);
INSERT INTO fooble VALUES(1, 'A little comment');
INSERT INTO fooble VALUES(2, 'Another great comment');

CREATE INDEX fooble_idx ON FOOBLE(comment);

CREATE TABLE bizzles(id INTEGER, fref INTEGER);
INSERT INTO bizzles VALUES(1, 1);
INSERT INTO bizzles VALUES(2, 2);
INSERT INTO bizzles VALUES(3, 1);
INSERT INTO bizzles VALUES(4, 1);
INSERT INTO bizzles VALUES(5, 1);
INSERT INTO bizzles VALUES(6, 1);
INSERT INTO bizzles VALUES(7, 1);
INSERT INTO bizzles VALUES(8, 1);
INSERT INTO bizzles VALUES(9, 1);
INSERT INTO bizzles VALUES(10, 1);
INSERT INTO bizzles VALUES(11, 1);
INSERT INTO bizzles VALUES(12, 1);
INSERT INTO bizzles VALUES(13, 1);
INSERT INTO bizzles VALUES(14, 1);
INSERT INTO bizzles VALUES(15, 1);
INSERT INTO bizzles VALUES(16, 1);
INSERT INTO bizzles VALUES(17, 1);
INSERT INTO bizzles VALUES(18, 1);
INSERT INTO bizzles VALUES(19, 1);
INSERT INTO bizzles VALUES(20, 1);
