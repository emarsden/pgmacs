# PGmacs -- Emacs editing PostgreSQL databases

<img src="doc/src/img/PGmacs-logo.svg"
     alt="PGmacs logo"
     style="width:15em;display:block;margin:auto">

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Latest tagged version](https://img.shields.io/github/v/tag/emarsden/pgmacs?label=Latest%20tagged%20version)](https://github.com/emarsden/pgmacs/)
[![Container size](https://ghcr-badge-hwb3.onrender.com/emarsden/pgmacs/size?label=Container%20image)](https://github.com/users/emarsden/packages/container/package/pgmacs)
[![Documentation build](https://img.shields.io/github/actions/workflow/status/emarsden/pgmacs/mdbook.yml?label=Documentation)](https://github.com/emarsden/pgmacs/actions/)
![Beta status](https://img.shields.io/badge/status-beta-blue)


PGmacs provides an editing interface for the PostgreSQL 🐘 object-relational DBMS from Emacs.
It allows you to:

- browse the **list of tables** in the database

- browse the contents of a table, row by row, in paginated mode for large tables, and navigate
  between tables if they are linked by a foreign key

- edit the value of a column: type `RET` on the value you want to modify to edit the value in the
  minibuffer, or type `w` to edit the value in a widget-based buffer

- insert new rows into the table: type `+` in a table buffer to insert a row with new values
  obtained from the minibuffer, or type `i` to insert a new row with values obtained from a
  dedicated widget-based buffer.

- delete a row: type `DEL` in a table buffer to delete the row at point

- copy/paste rows of a database table (type `k` to copy, `y` to paste in a table display buffer)

- save the contents of a table in CSV or TSV format, or the contents of a row in JSON format

It works both in the **terminal** (TUI) and in **GUI mode**.

![GIF editing](doc/src/img/edit-value.gif)


📖 You may be interested in the [user manual](https://emarsden.github.io/pgmacs/).


## Getting started

> [!TIP]
> If you want to get a quick feel for what PGmacs can do before installing it, you can try out our
> [prebuilt Podman/Docker container image](https://emarsden.github.io/pgmacs/container.html) which
> includes a terminal-only build of Emacs and the necessary dependencies.


In your Emacs initialization file, include the following to check out the latest version of the code
from the git repository, as well as the [pg-el dependency](https://github.com/emarsden/pg-el/):

    ;; Requires Emacs 29 and git
    (unless (package-installed-p 'pg)
       (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
    (unless (package-installed-p 'pgmacs)
       (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

    (require 'pgmacs)


You can later upgrade these to the latest version with `M-x package-vc-upgrade RET pgmacs RET`. See
the [user manual](https://emarsden.github.io/pgmacs/quickstart.html) for more installation methods
(for example using `use-package`).

To load PGmacs, say 

    M-x pgmacs
    
which will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). It will then open the PGmacs main buffer, which will show you a list of the tables
available in the database.

You can also open PGmacs with a PostgreSQL connection string

    M-x pgmacs-open-string RET user=myself port=5432 dbname=mydb

or with a PostgreSQL connection URI

    M-x pgmacs-open-uri RET postgresql://%2Fvar%2Flib%2Fpostgresql/dbname

or with a connection object from the pg.el library (function `pgmacs-open`). Check the [user
manual](https://emarsden.github.io/pgmacs/) for more. 


## Production-ready? 

PGmacs is in **beta status**. As of 2024-07, the author has sufficient confidence in the code to use
it to modify real PostgreSQL databases used in production.


## Supported platforms

**Emacs version**: PGmacs requires Emacs version 29. It is tested mostly on Emacs 30.1 and 29.4. It
has mostly been tested on Linux, but should work as expected on Microsoft Windows and MacOS. It
works both in graphical mode and in the terminal.

**PostgreSQL version**: PGmacs is primarily tested with PostgreSQL versions 17.5 and 16.4, but
should work with any PostgreSQL version supported by the `pg-el` library that it uses to communicate
with PostgreSQL. For example, it works fine with PostgreSQL version 14 which was released in 2021.

PGmacs also works, more or less, with some “**PostgreSQL-compatible**” databases. There are four main
points where this compatibility may be problematic:

- Compatibility with the PostgreSQL wire protocol. This is the most basic form of compatibility.

- Compatibility with the PostgreSQL flavour of SQL, such as row expressions, non-standard functions
  such as `CHR`, data types such as `BIT`, `VARBIT`, `JSON` and `JSONB`, user-defined ENUMS and so
  on, functionality such as `LISTEN`. Some databases that claim to be “Postgres compatible” don’t
  even support foreign keys, views, triggers, sequences, tablespaces and temporary tables (looking
  at you, Amazon Aurora DSQL).

- Implementation of the system tables that are used by PGmacs to retrieve the list of tables in a
  database, their on-disk size, column metainformation, and the list of indexes present.

- Establishing encrypted TLS connection to hosted services. Most PostgreSQL client libraries (in
  particular the official client library libpq) use OpenSSL for TLS support, whereas Emacs uses
  GnuTLS, and you may encounter incompatibilities.

The following PostgreSQL-compatible databases have been tested:

- [Neon](https://neon.tech/) “serverless PostgreSQL” works perfectly.

- [ParadeDB](https://www.paradedb.com/) seems to work fine in limited testing (it's really a
  PostgreSQL extension rather than a fully separate product). Last tested with v0.9.1.
  
- The [Timescale DB](https://www.timescale.com/) extension for time series data works perfectly
  (last tested with version 2.16.1).

- The [CitusDB](https://github.com/citusdata/citus) extension for sharding PostgreSQL over multiple
  hosts (AGPLv3 licence) works perfectly (last tested 2025-07 with Citus version 13.0).

- The [IvorySQL](https://www.ivorysql.org/) Oracle-compatible flavour of PostgreSQL works perfectly
  (last tested with version 3.4).

- The [Microsoft DocumentDB](https://github.com/microsoft/documentdb) extension for MongoDB-like
  queries works perfectly. Note that this is not the same product as Amazon DocumentDB.

- The [PgBouncer](https://www.pgbouncer.org/) connection pooler for PostgreSQL works fine (last
  tested with version 1.23 in the default session pooling mode).

- [Google AlloyDB Omni](https://cloud.google.com/alloydb/omni/docs/quickstart) is a proprietary fork
  of PostgreSQL with Google-developed extensions, including a columnar storage extension, adaptive
  autovacuum, and an index advisor. It works perfectly with PGmacs as of 2025-07.

- [YugabyteDB](https://yugabyte.com/) works to a limited extent: we are not able to run the SQL
  command that adds a PRIMARY KEY to an existing table, nor to display total database size on disk,
  for example. It does support some extensions such as pgvector, for example. Last tested 2025-07
  with v2.25.
  
- [CrateDB](https://crate.io/) works with limited functionality: for example querying the list of
  defined procedures and functions triggers an internal error in CrateDB. Last tested 2025-07 with
  v5.10.10.

- [CockroachDB](https://github.com/cockroachdb/cockroach) works with limited functionality: for
  example the list of defined procedures and functions is not properly populated. Lasted tested
  2025-07 with version 25.2.

- The [RisingWave](https://github.com/risingwavelabs/risingwave) event streaming database (Apache
  license) is mostly working. It does not support `GENERATED ALWAYS AS IDENTITY` or `SERIAL`
  columns, nor `VACUUM ANALYZE`. The database does not implement column renaming. Last tested
  2025-07 with v2.4.3.

- [Xata](https://xata.io/) “serverless PostgreSQL” has many limitations including lack of support
  for `CREATE DATABASE`, `CREATE COLLATION`, for XML processing, for temporary tables, for cursors,
  for `EXPLAIN`, for `CREATE EXTENSION`, for functions such as `pg_notify`.

- [QuestDB](https://questdb.io/) has very limited PostgreSQL support, and does not support the
  `integer` type for example. Last tested 2024-04 against version 8.3.3.

- [Google Spanner](https://cloud.google.com/spanner), or at least the Spanner emulator (that reports
  itself as `PostgreSQL 14.1`) and the PGAdapter library that enables support for the PostgreSQL
  wire protocol, is supported with limited functionality by PGmacs. Spanner has only limited
  PostgreSQL compatibility, for example refusing to create tables that do not have a primary key.

- The [Materialize](https://materialize.com/) operational database (a proprietary differential
  dataflow database) has many limitations in its PostgreSQL compatibility: no support for primary
  keys, unique constraints, check constraints, for the 'bit' type for example. It works with these
  limitations with PGmacs (last tested 2025-07 with Materialize v0.149).

- [YDB by Yandex](https://ydb.tech/docs/en/postgresql/docker-connect) has limited PostgreSQL
  compatibility (for example, it does not support foreign key references), but works with limited
  functionality with PGmacs. Lasted tested 2025-07 with version 23-4.

- ClickHouse does not work: its implementation of the wire protocol is very limited, with no support
  for the `pg_type` metadata and no support for basic PostgreSQL-flavoured SQL commands such as
  `SET`. Last tested with v24.5.

- Hosted PostgreSQL services that have been tested: as of 2025-07 render.com is running a Debian
  build of PostgreSQL 16.8 and works fine (requires TLS connection);
  [Railway.app](https://railway.app/) is running a Debian build of PostgreSQL 16.4, and works fine;
  [Aiven.io](https://aiven.io/) is running a Red Hat build of PostgreSQL 16.4 on Linux/Aarch64 and
  works fine.

- Untested but likely to work: Amazon RDS, Google Cloud SQL, Azure Database for PostgreSQL, Amazon
  Aurora. You may however encounter difficulties with TLS connections, as noted above.


## License

PGmacs is distributed under the terms of the GNU General Public License, version 3.

Copyright 2023-2025 Eric Marsden.

