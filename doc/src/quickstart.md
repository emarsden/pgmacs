# Quickstart

~~~admonish note title="Try it out before installing"
If you want to get a quick feel for what PGmacs can do before installing it, you can try out our
[prebuilt Podman/Docker container image](container.html) which includes a terminal-only build of
Emacs and the necessary dependencies. This will allow you to run PGmacs safely sandboxed in a
container.

Because it’s running in terminal mode, some functionality such as the SchemaSpy diagram support
won’t work in the container.
~~~


## Installation

In your Emacs initialization file, include the following to check out the latest version of the code
from the git repository, as well as the dependency [pg-el](https://github.com/emarsden/pg-el/):

```lisp
;; Requires Emacs 29 and git
(unless (package-installed-p 'pg)
   (package-vc-install "https://github.com/emarsden/pg-el/" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
   (package-vc-install "https://github.com/emarsden/pgmacs/"))

(require 'pgmacs)
```

You can later upgrade PGmacs to the latest version with `M-x package-vc-upgrade RET pgmacs RET`.

**With `use-package`**: with the vc support in the `use-package` macro (available from Emacs 29),
you can say

```lisp
(use-package pg :vc (:url "https://github.com/emarsden/pg-el/"))
(use-package pgmacs :vc (:url "https://github.com/emarsden/pgmacs/"))
```

**With [quelpa](https://github.com/quelpa/quelpa)**: 

```lisp
(use-package pgmacs
  :ensure nil
  :defer t
  :quelpa (pgmacs :fetcher github :repo "emarsden/pgmacs"))
```

**Manual installation**:

Clone this repository, ensure that directory is on your load path, as [described on
EmacsWiki](https://www.emacswiki.org/emacs/LoadPath), and say `(require 'pgmacs)`.

```shell
git clone https://github.com/emarsden/pgmacs/
```


## Setting up a PostgreSQL user with limited privileges

You should be careful about giving random software you downloaded from the internet access to your
PostgreSQL data. I would recommend you take a quick read through the source code (it’s quite short,
only 2600 lines) before running it. Before taking the time to do this, you can also run PGmacs as a
PostgreSQL user which is not allowed to insert or delete data. Here’s how to do this (using
predefined roles that are available from PostgreSQL v14 onwards): 

```sql
CREATE USER pgmacs_readonly_user WITH PASSWORD 'changeme';
GRANT pg_read_all_data TO pgmacs_readonly_user;
```


## Connecting to a PostgreSQL database

With the pgmacs.el library loaded, say 

    M-x pgmacs
    
This will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). It reads initial values for these variables from the following environment variables,
if they are set:

- `POSTGRES_DATABASE`, `POSTGRESQL_DATABASE`, `POSTGRES_DB`, `PGDATABASE`
- `POSTGRES_HOSTNAME`, `PGHOST`
- `POSTGRES_PORT_NUMBER`, `POSTGRESQL_PORT_NUMBER`, `PGPORT`
- `POSTGRES_USER`, `POSTGRESQL_USERNAME`, `PGUSER`
- `POSTGRES_PASSWORD`, `POSTGRESQL_PASSWORD`, `PGPASSWORD`

These semi-standardized variable names are used by the [official Docker image for
PostgreSQL](https://hub.docker.com/_/postgres/), the more sophisticated [Bitnami PostgreSQL
image](https://registry.hub.docker.com/r/bitnami/postgresql)) and by the official psql PostgreSQL client.

![Screenshot connection widget](img/connect-widget-table-list.gif)

It will then open the main PGmacs [table-list buffer](table-list.html), which will show you a list
of the tables available in the database.

You can also open PGmacs with a PostgreSQL connection string

    M-x pgmacs-open-string RET user=myself port=5432 dbname=mydb

or with a PostgreSQL connection URI

    M-x pgmacs-open-uri RET postgresql://%2Fvar%2Flib%2Fpostgresql/dbname

or with a PostgreSQL connection object from the pg.el library, using function `pgmacs-open`.


![Screenshot table list](img/screenshot-overview.png)


