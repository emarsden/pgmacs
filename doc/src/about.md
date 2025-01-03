# PGmacs

<img src="img/PGmacs-logo.svg" 
     alt="PGmacs logo"
     style="width:15em;display:block;margin:auto">

An Emacs-based browser and editor for the PostgreSQL database.

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
![Beta status](https://img.shields.io/badge/status-beta-blue)



![Overview gif](img/edit-value.gif)


PGmacs provides an editing interface for the PostgreSQL 🐘 DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row, in paginated mode for large tables

- edit the value of a column (type <kbd>RET</kbd> on the value you want to modify)

- delete a row (type <kbd>DEL</kbd> on the row you wish to delete)

- copy/paste rows of a database table (type <kbd>k</kbd> to copy, <kbd>y</kbd> to paste in a table
  display buffer)

- export the contents of a table to CSV or TSV format

PGmacs works with Emacs running in a **terminal** (TUI), or running in your platform’s GUI **window
system**.


## Production-ready? 

PGmacs is in **beta status**. As of 2024-07, the author has sufficient confidence in the code to use
it to modify real PostgreSQL databases used in production.


## Supported versions

**Emacs version**: PGmacs requires Emacs version 29. It has also been tested on the
pre-release v30. It has mostly been tested on Linux, but should work as expected on Microsoft
Windows and MacOS. It works both in graphical mode and in the terminal.

~~~admonish note title="Better network performance with Emacs 31 (unreleased)"

When connecting to PostgreSQL over the network (rather than over a local Unix connection), you will
see far better performance using the unreleased Emacs 31. This version (which you will need to build
from source) supports disabling the Nagle algorithm (the `TCP_NODELAY` option on network sockets),
which increases performance by a factor of 12 when running the test suite.

~~~


**PostgreSQL version**: PGmacs is mostly tested with PostgreSQL versions 17.2 and 16.4, but should
work with any PostgreSQL version supported by the `pg-el` library that it uses to communicate with
PostgreSQL. For example, it works fine with PostgreSQL version 14 which was released in 2021.

PGmacs also works with some databases that implement the PostgreSQL frontend-backend protocol, but
not with all of them. PGmacs queries various internal PostgreSQL tables for metainformation on the
list of tables available, and these tables are not always present in PostgreSQL-compatible
databases. PGmacs also uses some PostgreSQL-specific functions to display information such as the
on-disk size of tables, and these functions are not always implemented. What we have tested so far:

- [Neon](https://neon.tech/) “serverless PostgreSQL” works perfectly.

- [ParadeDB](https://www.paradedb.com/) v0.9.1 seems to work fine in limited testing (it’s really a
  PostgreSQL extension rather than a fully separate product).
  
- [IvorySQL](https://www.ivorysql.org/) version 3.4 works perfectly (this fork of PostgreSQL adds
  some features for compatibility with Oracle).

- The [Timescale DB](https://www.timescale.com/) extension for time series data works perfectly
  (last tested with version 2.16.1).

- The [CitusDB](https://github.com/citusdata/citus) extension for sharding PostgreSQL over multiple
  hosts works perfectly (last tested with Citus version 12.1.5, which is based on PostgreSQL 16.6).

- The [Timescale DB](https://www.timescale.com/) extension for time series data works perfectly
  (tested with version 2.16.1).

- [Xata](https://xata.io/) “serverless PostgreSQL” has many limitations including lack of support
  for `CREATE DATABASE`, `CREATE COLLATION`, for XML processing, for temporary tables, for cursors,
  for `EXPLAIN`, for `CREATE EXTENSION`, for functions such as `pg_notify`.

- [YugabyteDB](https://yugabyte.com/) v2.23 works to a limited extent: we are not able to run the
  SQL command that adds a PRIMARY KEY to an existing table, nor to display total database size on
  disk, for example.
  
- [CrateDB](https://crate.io/) v5.8.5 is supported with some workarounds (it does not currently
  implement PostgreSQL functions and system tables that we use to query table metainformation, so
  some display features are limited).

- [CockroachDB](https://github.com/cockroachdb/cockroach) version 24.2 is supported with some
  limitations and workarounds (it does not currently implement PostgreSQL functions and system
  tables that we use to query table metainformation, or our queries generate internal errors, so
  some display features are limited).

- [QuestDB](https://questdb.io/): tested against version 6.5.4. This has very limited PostgreSQL
  support, and does not support the `integer` type for example.

- [Google Spanner](https://cloud.google.com/spanner), or at least the Spanner emulator (that reports
  itself as `PostgreSQL 14.1`) and the PGAdapter library that enables support for the PostgreSQL
  wire protocol, do not work with PGmacs. Spanner has only limited PostgreSQL compatibility, for
  example refusing to create tables that do not have a primary key. It does not implement some
  functions we use to query the current user and database status, such as `current_user`,
  `pg_backend_pid`, `pg_is_in_recovery`.

- [YDB by Yandex](https://ydb.tech/docs/en/postgresql/docker-connect) version 23-4 has very limited
  PostgreSQL compatibility and does not work with PGmacs. The system tables that we query to obtain
  the list of tables in the current database are not implemented.

- ClickHouse v24.5 does not work: its implementation of the PostgreSQL wire protocol is very
  limited, with no support for the `pg_type` metadata and no support for basic PostgreSQL-flavoured
  SQL commands such as `SET`.


## License

PGmacs is distributed under the terms of the GNU General Public License, version 3.

Copyright 2023-2025 Eric Marsden.

