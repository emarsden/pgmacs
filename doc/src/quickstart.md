# Quickstart

In your Emacs initialization file, include the following to check out the latest version of the code
from the git repository, as well as the [pg-el dependency](https://github.com/emarsden/pg-el/):

    ;; this not necessary if using an Emacs 30 pre-release
    (unless (package-installed-p 'vc-use-package)
      (package-vc-install "https://github.com/slotThe/vc-use-package"))
    (require 'vc-use-package)

    (use-package pg
      :vc (:fetcher github :repo emarsden/pg-el))
    (use-package pgmacs
      :vc (:fetcher github :repo emarsden/pgmacs))


## Connecting to a PostgreSQL database

With the pgmacs.el library loaded, say 

    M-x pgmacs
    
This will open a widget-based buffer to collect connection information (database name, hostname,
port, etc.). 

![Screenshot connection widget](img/connect-widget-table-list.gif)

It will then open the PGmacs main buffer, which will show you a list of the tables available in the
database.

You can also open PGmacs with:

- a PostgreSQL connection object from the pg.el library, using function `pgmacs-open`

- a PostgreSQL connection string such as `user=myself port=5432 dbname=mydb`, using function
  `pgmacs-open-string`

- a PostgreSQL connection URI such as `postgresql://%2Fvar%2Flib%2Fpostgresql/dbname`, using
  function `pgmacs-open-uri`.


![Screenshot table list](img/screenshot-overview.png)



## The table list buffer

The table list buffer is the main PGmacs buffer. It shows some metainformation concerning the
PostgreSQL backend that you are connected to (version, database size, etc.), followed by a table
which includes one row per table in the database. 

The following keys are bound when the point is located in the table list buffer: 

| Key       | Binding                                                                              |
|-----------|--------------------------------------------------------------------------------------|
| RET       | Open a new buffer to browse/edit the table at point.                                 |
| DEL       | Delete the table at point.                                                           |
| r         | Renamee the table at point.                                                          |
| e         | Open a new buffer to display the result of an SQL query.                             |
| <         | Move to the beginning of the table list.                                             |
| >         | Move to the end of the table list.                                                   |
| {         | Make the current column narrower.                                                    |
| }         | Make the current column wider.                                                       |
| q         | Bury the current buffer.                                                             |
