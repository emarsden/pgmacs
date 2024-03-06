# pgmacs.el -- Emacs editing PostgreSQL databases

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Alpha status](https://img.shields.io/badge/status-alpha-blue)


PGMacs provides an editing interface for the PostgreSQL 🐘 object-relational DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row

- edit the value of a column (type `RET` on the value you want to modify)

- copy/paste rows of a database table (type `k` to copy, `y` to paste in a table display buffer)


## Get started

You will need the [pg-el library](https://github.com/emarsden/pg-el/) installed, available in
[MELPA](https://melpa.org/):

    M-x package-install RET pg

Load this library, then say 

    M-x pgmacs-open-db
    
which will prompt you for a database name and a user. To specify additional arguments such as a
password, a host other than `localhost` and a port other than 5432, call the function

    (pgmacs-open (pg-connect "database-name" "username" "password "localhost" 5433))

This should show you a list of the tables available in the database.


## Status

PGMacs is in **alpha status**. Please only use it on test databases that do not contain important
data. 

The library has been tested with PostgreSQL version 16.2, but should work with any PostgreSQL
version supported by the `pg-el` library that it uses to communicate with PostgreSQL. The code has
been tested with Emacs 29.2 on Linux. It requires version 29.x because it uses the `vtable` library.


## License

PGMacs is distributed under the terms of the GNU General Public License, version 2.

Copyright 2023-2024 Eric Marsden.



The **latest version** of this package should be available from

    https://github.com/emarsden/pgmacs/
