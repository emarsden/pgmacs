# pgmacs.el -- Emacs editing PostgreSQL databases

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Beta status](https://img.shields.io/badge/status-beta-blue)


PGmacs provides an editing interface for the PostgreSQL 🐘 object-relational DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row, in paginated mode for large tables

- edit the value of a column: type `RET` on the value you want to modify to edit the value in the
  minibuffer, or type `w` to edit the value in a widget-based buffer

- insert new rows into the table: type `+` in a table buffer to insert a row with new values
  obtained from the minibuffer, or type `i` to insert a new row with values obtained from a
  dedicated widget-based buffer.

- delete a row: type `DEL` in a table buffer to delete the row at point

- copy/paste rows of a database table (type `k` to copy, `y` to paste in a table display buffer)

- save the contents of a table in CSV or TSV format

It works both in the **terminal** and in **GUI mode**.

![GIF editing](doc/src/img/edit-value.gif)



## Getting started

In your Emacs initialization file, include the following to check out the latest version of the code
from the git repository, as well as the [pg-el dependency](https://github.com/emarsden/pg-el/):

    ;; Requires Emacs 29 and git
    (unless (package-installed-p 'pg)
       (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
    (unless (package-installed-p 'pgmacs)
       (package-vc-install "https://github.com/emarsden/pgmacs"))

    (require 'pgmacs)


You can later upgrade these to the latest version with `M-x package-vc-upgrade RET pgmacs RET`.

To load PGmacs, say 

    M-x pgmacs
    
which will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). It will then open the PGmacs main buffer, which will show you a list of the tables
available in the database.

You can also open PGmacs with a connection object from the pg.el library (function `pgmacs-open`),
or with a PostgreSQL connection string

    M-x pgmacs-open-string RET user=myself port=5432 dbname=mydb

or with a PostgreSQL connection URI

    M-x pgmacs-open-uri RET postgresql://%2Fvar%2Flib%2Fpostgresql/dbname

Check the [user manual](https://emarsden.github.io/pgmacs/) for more.


## Supported platforms

PGmacs is in **beta status**. Please only use it on test databases that do not contain important
data. 

**Emacs version**: PGmacs requires Emacs version 29. It has also been tested on the
pre-release v30. It has mostly been tested on Linux, but should work as expected on Microsoft
Windows and MacOS. It works both in graphical mode and in the terminal.

**PostgreSQL version**: PGmacs has been tested with PostgreSQL version 16.3 and 17beta1, but should
work with any PostgreSQL version supported by the `pg-el` library that it uses to communicate with
PostgreSQL. For example, it works fine with PostgreSQL version 14 which was released in 2021.

PGmacs also works with some databases that implement the PostgreSQL frontend-backend protocol, but
not with all of them. PGmacs queries various internal PostgreSQL tables for metainformation on the
list of tables available, and these tables are not always present in PostgreSQL-compatible
databases. PGmacs also uses some PostgreSQL-specific functions to display information such as the
on-disk size of tables, and these functions are not always implemented. What we have tested so far:

- ParadeDB v0.7.3 sees to work fine in limited testing (it's more a PostgreSQL extension than a
  fully separate product).
  
- YugabyteDB v2.21 works to a limited extent: we are not able to run the SQL command that adds a
  PRIMARY KEY to an existing table, nor to display total database size on disk, for example. 
  
- CrateDB v5.7 does not currently work; it does not implement PostgreSQL functions that we use to
  query table metainformation.

- ClickHouse v24.5 does not work: its implementation of the wire protocol is very limited, with no
  support for the `pg_type` metadata.


## License

PGmacs is distributed under the terms of the GNU General Public License, version 2.

Copyright 2023-2024 Eric Marsden.

