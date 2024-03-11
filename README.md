# pgmacs.el -- Emacs editing PostgreSQL databases

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Alpha status](https://img.shields.io/badge/status-alpha-blue)


PGMacs provides an editing interface for the PostgreSQL 🐘 object-relational DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row, in paginated mode for large tables

- edit the value of a column (type `RET` on the value you want to modify)

- copy/paste rows of a database table (type `k` to copy, `y` to paste in a table display buffer)

![Screenshot table list](doc/src/img/screenshot-overview.png)



## Getting started

You will need the [pg-el library](https://github.com/emarsden/pg-el/) installed, available in
[MELPA](https://melpa.org/):

    M-x package-install RET pg

Load this library, then say 

    M-x pgmacs
    
which will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). It will then open the PGMacs main buffer, which will show you a list of the tables
available in the database.

You can also open pgmacs with a connection object from the pg.el library (function `pgmacs-open`),
or with a PostgreSQL connection string such as `user=myself port=5432 dbname=mydb` (function
`pgmacs-open/string`) or with a PostgreSQL connection URI such as
`postgresql://%2Fvar%2Flib%2Fpostgresql/dbname` (function `pgmacs-open/uri`). 


## Status

PGMacs is in **alpha status**. Please only use it on test databases that do not contain important
data. 

The library has been tested with PostgreSQL version 16.2, but should work with any PostgreSQL
version supported by the `pg-el` library that it uses to communicate with PostgreSQL. The code has
been tested with Emacs 29.2 on Linux. It requires version 29.x because it uses the `vtable` library.


## License

PGMacs is distributed under the terms of the GNU General Public License, version 2.

Copyright 2023-2024 Eric Marsden.

