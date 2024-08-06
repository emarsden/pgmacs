# Changelog

## [0.12] - Unreleased

- Typing `!` in a row-list buffer runs a shell-command on the current cell value, and displays the
  output in the echo area. If called with a prefix argument, it replaces the current cell value with
  the output of the cell command, and also updates the PostgreSQL database.

  For example, to count the length in characters of the current cell value (or of its displayed
  representation, when it is not a text field), type

```shell
! wc -c
```

  To reverse the characters in the current cell, and also update the database:

```shell
C-u ! rev
```

  To downcase the characters in the current cell, and also update the database:

```shell
C-u ! tr '[:upper:]' '[:lower:']
```

- New keybindings to upcase (`M-u`), downcase (`M-l`) and capitalize (`M-c`) the contents of the
  current cell, and update the database.

- Typing `R` in a row-list table will rename the current table column. The new name will be read
  from the minibuffer.

- Preliminary support for running the SchemaSpy application on the current table and displaying the
  schema diagram. See customizable variable `pgmacs-schemaspy-cmdline`. 

- Preliminary support for a customized header line (see customizable variable `pgmacs-header-line`).

- New customizable variable `pgmacs-enable-query-logging` specifies whether SQL queries should be
  logged to a dedicated buffer. The default value is false.


## [0.11] - 2024-07-30

- Speed optimization: the complex queries used to analyze the information_schema tables to identify
  the constraints on a table column (CHECK constraints, maximum length constraints, FOREIGN KEY
  constraints) are saved as prepared statements to be reused by later calls. This should speed up
  the generation of row-list buffers. This change requires the latest release of the pg-el library.

- In a row-list buffer, typing `RET` on a column that references a foreign key will open the
  referenced table and position the cursor on the referenced row. To force an edit (the normal
  behaviour for `RET` in a row-list buffer) you can type `w`.

- New functionality to run SQL from an Emacs buffer and display the output. Bound to `E` (the
  existing functionality to run SQL read from the minibuffer is bound to `e`).

- New customizable face `pgmacs-column-primary-key` (defaults to the bold of the standard table data
  face) is used to display a table column that is part of a primary key.

- New text button affordances to add/modify table comments in row-list buffers.

- Improve keymap handling: bindings in `pgmacs-table-list-map`, `pgmacs-row-list-map`,
  `pgmacs-transient-map` and our minor keymap `pgmacs-paginated-map` also apply outside of the
  pgmacstbl.

- The beginnings of a basic menu-bar, with entries for open-uri and open-string.

- Rename function `pgmacs-display-table` to `pgmacs-open-table`.

- Change licence from GPL v2-or-later to GPL v3-or-later.


## [0.10] - 2024-07-21

- Include the content of CHECK constraints in the column metainformation shown for a table buffer.

- Fix the SQL query used to identify a column that references a foreign key (displayed using
  `pgmacs-column-foreign-key` face, which defaults to blue).

- Fix “kill row as JSON” for certain column types, which need serialization before being converted
  to JSON.

- Display: use ASCII alternatives when box-drawing characters are not displayable.

- Display: encourage Emacs to use 256 colors when running in terminal mode in the prebuilt software
  container.


## [0.9] - 2024-07-06

- Columns that contain data that references another table (`REFERENCES parent(col)` or `FOREIGN
  KEY(fkcols)`) are displayed using face `pgmacs-column-foreign-key` (defaults to a blue foreground
  color).

- In the `*PostgreSQL backend information*` buffer, add a "Install extension" button for extensions
  that are available but not installed in the current database.

- When a CSV dump of a table is inserted into a buffer, put the buffer in `csv-mode` if that package
  is installed.

- Publish a prebuilt software container with Emacs + PGmacs + dependencies.


## [0.8] - 2024-06-29

- Look and feel: adopt the button faces used by Emacs’ customization support.

- New widgets to allow widget-based editing of a PostgreSQL JSON or JSONB value, a DATE values and
  UUID values.

- When displaying a table, show whether row-level security is enabled for this table.

- Row deletion is now executed in an SQL transaction. If the number of affected rows is not equal to
  1 (indicating a logic error in our code), then the transaction is rolled back.

- A `Count rows` button is displayed in table row-list buffers.

- The connection widget uses initial values for database name, username, hostname and password taken
  from environment variables such as `POSTGRES_DATABASE` and `POSTGRES_USER`, if defined.

- Add per-session history for `pgmacs-open-string` and `pgmacs-open-uri`.


## [0.7] - 2024-05-23

- Fix bug in `e` keybinding (`pgmacs-run-sql`) when outside a table.

- `pgmacs-run-sql`: allow for SQL commands that produce no output rows.

- In a row-list, new keybindings for digits that move point to the nth column of the table, counting
  from zero. For example, pressing `2` moves point to the third column.

- In a row-list, new `v` keybinding displays the column value at point (which may be truncated) in a
  dedicated (readonly) buffer.

- Fix bug in the the widget-based editing functionality, which was discarding edits.

- New widget to allow widget-based editing of a PostgreSQL HSTORE value, which is represented in
  Emacs Lisp as a hashtable.


## [0.6] - 2024-05-09

- New keybinding in the table-list buffer: `r` allows you to rename the table at point.

- New keybinding in the table-list buffer: `g` refreshes the table list display by retrieving all
  the table metainformation again.

- Improve the display of NULL column values.


## [0.5] - 2024-04-27

- Information on available and installed PostgreSQL extensions is included in the buffer generated
  by `pgmacs--display-backend-information`.

- API change: `pgmacs-open/string` and `pgmacs-open/uri` renamed to `pgmacs-open-string` and
  `pgmacs-open-uri` to follow the conservative Emacs Lisp style guide.


## [0.4] - 2024-04-05

- The row count shown in the list-of-tables buffer is now precise even when the tables have not been
  VACUUMed. This precision is at the cost of speed on large tables (calculated with `COUNT(*)`).

- Add `h` keybinding in row-list and table-list buffers that show a buffer with the main keybindings.

- New keybindings `<` and `>` in row-list and table-list buffers to move to the beginning and the
  end of the vtable, respectively.


## [0.3] - 2024-03-31

- The comment on a table can be modified by pressing `RET` in the list-of-tables buffer.

- Alignment of table headers should be improved, both in a window system and a terminal.

- New keybinding for `<delete>` and `<backspace>` in the list-of-tables buffer, which allow easy
  deletion of tables after `yes-or-no-p` confirmation.


## [0.2] - 2024-03-29

- New functions `pgmacs-open/string` to open PGmacs with a PostgreSQL connection string, and
  `pgmacs-open/uri` to open PGmacs with a PostgreSQL connection URI.

- New function `pgmacs` which opens a widget-based buffer to enter PostgreSQL connection information.

- `e` in keymap reads an SQL query from the minibuffer and displays the output in a temporary buffer.

- `j` in a table view copies the current row to the kill ring in JSON format.

- Pressing `k` in a table view copies the current row to a special kill ring. Pressing `y` then
  pastes the copied row into the table, taking care to use defaults for any colums for which a
  schema-specified default value is defined.

- In a table view for which pagination is active (only a subset of the table/query contents are
  displayed in the buffer), pressing `n` and `p` updates the table data to display the next and
  previous pages respectively.

- Pressing `+` in a table view allows you to insert a new row, with values for each column entered
  in the minibuffer. Columns for which an SQL default value is specified will use that default
  value.

- Pressing `i` in a table view allows you to insert a new row, with values for each column entered
  in a widget-based buffer. Columns for which an SQL default value is specified will use that default
  value.

- In table buffers, include a button that dumps the table as CSV to an Emacs buffer.

- Faces `pgmacs-table-header` and `pgmacs-table-data` are used to display the header and the rows of
  database tables.

- Variable `pgmacs-row-colors` specifies the colors used for alternating rows in a database table.

- Variable `pgmacs-row-limit` specifies the maximum number of rows to retrieve per database query,
  before results are paginated.

- Support for schema-qualified tables in the table list, with an updated version of the pg-el
  library.

- Some attempt to show query progress on slow connections to PostgreSQL using Emacs's
  `make-progress-reporter` functionality.
