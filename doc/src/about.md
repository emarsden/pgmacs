# PGmacs

An Emacs-based browser and editor for the PostgreSQL database.

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Beta status](https://img.shields.io/badge/status-beta-blue)



![Overview gif](img/edit-value.gif)


PGmacs provides an editing interface for the PostgreSQL 🐘 DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row, in paginated mode for large tables

- edit the value of a column (type `RET` on the value you want to modify)

- delete a row (type `DEL` on the row you wish to delete)

- copy/paste rows of a database table (type `k` to copy, `y` to paste in a table display buffer)

- export the contents of a table to CSV or TSV format

PGmacs works with Emacs running in a **terminal** (started with the `-nw` commandline option), or
running in your platform’s **window system**.


## Supported versions

PGmacs is in **beta status**. Please only use it on test databases that do not contain important
data. 

**Emacs version**: PGmacs requires Emacs version 29. It has also been tested on the
pre-release v30. It has mostly been tested on Linux, but should work as expected on Microsoft
Windows and MacOS. It works both in graphical mode and in the terminal.

**PostgreSQL version**: PGmacs has been tested with PostgreSQL versions 16.3 and 17beta1, but should
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



