<# pgmacs.el -- Emacs editing PostgreSQL databases

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Alpha status](https://img.shields.io/badge/status-alpha-blue)


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

![GIF editing](doc/src/img/edit-value.gif)



## Getting started

In your Emacs initialization file, include the following to check out the latest version of the code
from the git repository, as well as the [pg-el dependency](https://github.com/emarsden/pg-el/):

    ;; Requires Emacs 29 and git
    (package-vc-install "https://github.com/emarsden/pg-el")
    (package-vc-install "https://github.com/emarsden/pgmacs")

You can later upgrade these to the latest version with `M-x package-vc-upgrade RET pgmacs RET`.

To load PGmacs, say 

    M-x pgmacs
    
which will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). It will then open the PGmacs main buffer, which will show you a list of the tables
available in the database.

You can also open PGmacs with a connection object from the pg.el library (function `pgmacs-open`),
or with a PostgreSQL connection string such as `user=myself port=5432 dbname=mydb` (function
`pgmacs-open-string`) or with a PostgreSQL connection URI such as
`postgresql://%2Fvar%2Flib%2Fpostgresql/dbname` (function `pgmacs-open-uri`). 

Check the [user manual](https://emarsden.github.io/pgmacs/) for more.


## Status

PGmacs is in **alpha status**. Please only use it on test databases that do not contain important
data. 

The library has been tested with PostgreSQL version 16.2, but should work with any PostgreSQL
version supported by the `pg-el` library that it uses to communicate with PostgreSQL. The code has
mostly been tested with Emacs 29.2 on Linux, but should work as expected on Microsoft Windows and
MacOS. It works both in graphical mode and in the terminal.


## License

PGmacs is distributed under the terms of the GNU General Public License, version 2.

Copyright 2023-2024 Eric Marsden.

