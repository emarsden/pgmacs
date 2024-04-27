# PGmacs

An Emacs browser and editor for the PostgreSQL database.

[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Alpha status](https://img.shields.io/badge/status-alpha-blue)



![Overview gif](img/edit-value.gif)


PGmacs provides an editing interface for the PostgreSQL 🐘 object-relational DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row, in paginated mode for large tables

- edit the value of a column (type `RET` on the value you want to modify)

- copy/paste rows of a database table (type `k` to copy, `y` to paste in a table display buffer)

- export the contents of a table to CSV or TSV format

PGmacs works with Emacs running in a terminal (started with the `-nw` commandline option), or
running in your platform’s window system.


## Status

PGmacs is in **alpha status**. Please only use it on test databases that do not contain important
data. 

The library has been tested with PostgreSQL version 16.2, but should work with any PostgreSQL
version supported by the `pg-el` library that it uses to communicate with PostgreSQL. The code has
mostly been tested with Emacs 29.2 on Linux, but should work as expected on Microsoft Windows and
MacOS.


## License

PGmacs is distributed under the terms of the GNU General Public License, version 2.

Copyright 2023-2024 Eric Marsden.



The **latest version** of this package should be available from

    https://github.com/emarsden/pgmacs/


