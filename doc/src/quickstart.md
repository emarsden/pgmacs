# Quickstart

In your Emacs initialization file, include the following to check out the latest version of the code
from the git repository, as well as the dependency [pg-el](https://github.com/emarsden/pg-el/):

    ;; Requires Emacs 29 and git
    (unless (package-installed-p 'pg)
       (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
    (unless (package-installed-p 'pgmacs)
       (package-vc-install "https://github.com/emarsden/pgmacs"))

    (require 'pgmacs)

You can later upgrade PGmacs to the latest version with `M-x package-vc-upgrade RET pgmacs RET`.

**With `use-package`**: if you prefer the `use-package` macro (which is integrated with Emacs 29),
you can instead say

    (use-package pg :vc (:url "https://github.com/emarsden/pg-el"))
    (use-package pgmacs :vc (:url "https://github.com/emarsden/pgmacs"))


## Connecting to a PostgreSQL database

With the pgmacs.el library loaded, say 

    M-x pgmacs
    
This will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). It reads initial values for these variables from the environment variables
`POSTGRES_DATABASE`, `POSTGRES_HOSTNAME`, `POSTGRES_PORT_NUMBER`, `PGPORT`, `POSTGRES_USER`,
`POSTGRESQL_USERNAME`, `POSTGRES_PASSWORD` and `POSTGRESQL_PASSWORD`, if they are defined (these
semi-standardized variable names are used by the [official Docker image for
PostgreSQL](https://hub.docker.com/_/postgres/) and the more sophisticated [Bitnami PostgreSQL
image](https://registry.hub.docker.com/r/bitnami/postgresql)).

![Screenshot connection widget](img/connect-widget-table-list.gif)

It will then open the PGmacs main buffer, which will show you a list of the tables available in the
database.

You can also open PGmacs with a PostgreSQL connection string

    M-x pgmacs-open-string RET user=myself port=5432 dbname=mydb

or with a PostgreSQL connection URI

    M-x pgmacs-open-uri RET postgresql://%2Fvar%2Flib%2Fpostgresql/dbname

or with a PostgreSQL connection object from the pg.el library, using function `pgmacs-open`.


![Screenshot table list](img/screenshot-overview.png)



## The table list buffer

The table list buffer is the main PGmacs buffer. It shows some metainformation concerning the
PostgreSQL backend that you are connected to (version, database size on disk, etc.), followed by a
table which includes one row per table in the database.

The following keys are bound when the point is located in the table list buffer: 

| Key   | Binding                                                  |
|-------|----------------------------------------------------------|
| `RET` | Open a new buffer to browse/edit the table at point.     |
| `DEL` | Delete the table at point.                               |
| `r`   | Rename the table at point.                               |
| `e`   | Open a new buffer to display the result of an SQL query. |
| `<`   | Move to the beginning of the table list.                 |
| `>`   | Move to the end of the table list.                       |
| `{`   | Make the current column narrower.                        |
| `}`   | Make the current column wider.                           |
| `q`   | Bury the current buffer.                                 |
