# Changelog


## [0.26] - 2025-07-09

- Use keymaps for all key bindings, to allow customization by the user. There are customizable
  keymaps for the table-list buffer (`pgmacs-table-list-map`), row-list buffers
  (`pgmacs-row-list-map`) and proc-list buffers that show the functions and procedures available
  (`pgmacs-proc-list-map`). These keymaps have child keymaps for keybindings that are only relevant
  when point is in the tabular data display; for example `pgmacs-row-list-map/table` contains
  keybindings that act on a specific cell value or move to a specific table column.

- Add workarounds for showing current database settings with PostgreSQL variant YDB (work around the
  missing two-parameter version of `current_setting` function).

- Fix sorting by size on disk in the main table-list buffer.

- In the procedure-list buffer, the `DEL` key allows you to delete the function or procedure at point.

- When serializing a row to JSON, NULL column values are handled correctly.


## [0.25] - 2025-06-08

- Table on disk size information is now printed for QuestDB and CrateDB, using specific per-variant
  SQL queries.

- In row-list buffers, add buttons in the column metainformation section allowing the user to rename
  a column and to add or remove a column comment.

- New customizable variables `pgmacs-row-list-table-name-width` and `pgmacs-row-list-comment-width`
  allow the user to specify the number of characters to be used to display table names and table
  comments, in the table-list buffer.

- New hook `pgmacs-row-list-hook` allows the user to specify functions to be run after opening or
  refreshing the table-list buffer.

- In the backend-information buffer, redraw the buffer when the "Load extension" button is used, so
  that the extension status displayed is correct.


## [0.24] - 2025-04-19

- Further workarounds for semi-compatible PostgreSQL variants YDB, Materialize, Spanner and
  Risingwave.

- Send a `REFRESH` or `FLUSH` command after inserting a new row or updating data, for PostgreSQL
  variants that offer eventually-consistent semantics (at least CrateDB and Materialize).

- In row-list buffers, if a comment is present on a table column, it is displayed in the
  metainformation header.


## [0.23] - 2025-03-23

- Further workarounds for semi-compatible PostgreSQL variant Materialize.

- Updating the version requirement on the pg-el library to benefit from improved compatibility with
  multiple PostgreSQL variants: QuestDB, Spanner, YugabyteDB and CrateDB.

- When adding a PRIMARY KEY to a table, use a column of type `UUID` with an automatically generated
  random value, for PostgreSQL variants that do not support `GENERATED ALWAYS AS IDENTITY`. This is
  the case for different distributed databases, for which autoincremented integers are difficult to
  manage.


## [0.22] - 2025-02-22

- The progress reporter will automatically be stopped after 10 seconds, if it's still running. This
  ensures that it is stopped even when the connection to PostgreSQL fails.

- Widget-based editing is implemented for the `timestamp` and `timestamptz` types.

- Further workarounds for semi-compatible PostgreSQL variants QuestDB, CrateDB, CockroachDB, YDB and
  Spanner.

- A new shortcut button in the table-list buffer shows the list of databases visible to the current
  user.


## [0.21] - 2025-01-12

- New customizable user option `pgmacs-large-database-threshold`. For a database larger than this
  value (queried via `pg_database_size`), PGmacs will estimate table row counts using an imprecise
  method that does not require a full index (or table) scan, but will provide invalid results for
  tables that have not been VACUUMed or ANALYZEd. For sizes below this threshold, a more accurate
  `SELECT COUNT(*) FROM table_name` query will be used. If set to zero, full index/table scans will
  never be issued (this may be a good choice on large production databases). Problem noted by
  @akurth.

- Further workarounds for semi-compatible PostgreSQL variants that don't implement all the system
  tables that we query to obtain metainformation concerning sizes, constraints, comments and so on.
  PGmacs provides a more spartan display for such databases.

- Add a “Disconnect” button to the main PGmacs table list buffer (suggestion from @akurth).


## [0.20] - 2025-01-02

- New variable `pgmacs-row-list-buttons` which offers similar functionality to
  `pgmacs-table-list-buttons` for a row-list buffer. Add the ability to define buttons that are only
  shown if a particular condition is non-nil, thanks to the `:condition` initarg to a button.

- New customizable variables `pgmacs-timestamp-format` and `pgmacs-timestamp-zone`. The first is a
  format string used to display column data of type timestamp, timestamptz and datetime. The second
  is an optional time zone to use for the display of timestamp, timestamptz and datetime data; the
  default behaviour is to use the local timezone. Patch from @akurth.

- When reading SQL queries from the minibuffer, completion is available for SQL keywords.


## [0.19] - 2024-12-22

- Improve the display of the list of procedures/functions in the database. The display of the full
  procedure definition is moved to a separate buffer which is opened by typing `RET` in the
  proc-list buffer.

- New keybinding in table-list, row-list and proc-list buffers: `TAB` moves to the next column,
  first moving to the next line if already on the last column.

- Make better use of faces to improve support for light/dark background themes: new faces
  `pgmacs-highlighted` and `pgmacs-muted` that are used in our help buffers, and customizable
  `pgmacs-deleted-color` used for rows that are pending deletion. Legibility problem noted by
  @distichum.

- The row of text buttons displayed above the list of tables in the main PGmacs buffer is now
  extensible. Add an object that implements `pgmacs--insert` (such as a `pgmacs-shortcut-button`
  object) to the `pgmacs-table-list-buttons` list, and that will be included in the button
  shortcuts.

- Fix display of foreign key reference metadata for columns in row-list buffers: the information
  displayed for composite foreign key references (composed of multiple columns in either the source
  or the destination table) was incorrect (reported by @akurth). Foreign key information is only
  displayed in the column section when it concerns a simple key. An additional foreign key section
  is displayed under the column metadata, using similar formatting to the psql tool.


## [0.18] - 2024-12-04

- Redraw the table list after renaming a table.

- New customizable variable `pgmacs-row-list-hook` to specify functions to be run when a row-list
  buffer has been created.

- Optimization: don't use expensive `:eval` in the definition of `pgmacs-header-line`.

- Preliminary changes to work with certain semi-compatible PostgreSQL variants that don't implement
  all of the system tables or metadata functions that we use to display additional information
  concerning databases and tables. Currently preliminary changes for CrateDB, CockroachDB, Spanner,
  YDB.


## [0.17] - 2024-11-11

- Bug fix: adding a WHERE filter is possible when point is located outside the row-list table.

- Bug fix: regenerate the row color cache when inserting or deleting rows, to account for the
  changed row count.

- Bug fix: row-marks are correctly adjusted when inserting or deleting rows in a table.

- Bug fix: insertion of new row data of type HSTORE functions correctly.

- Bug fix: column name completion function does not generate errors when no pgmacstbl is present
  (which can occur when displaying an empty result set).

- Bug fix: updating the last row of a table no longer triggers an error related to row colors.

- When marking table rows for deletion, don’t move point to the next row if we are already on the
  last row.

- When paginating table display, fix the logic for display of “Previous rows” button.


## [0.16] - 2024-10-27

- New keybinding in a row-list buffer: `U` will unmark all marked rows (deselects them for
  deletion).

- New keybinding in a row-list buffer: `&` will run an application asynchronously with the current
  cell value as first commandline argument. This complements the existing `!` keybinding which runs
  a Unix-like filter shell command with the current cell value as input.

- Fixes to the interaction between marking rows for deletion and refetching/redrawing the row-list
  table. A refetch will now unmark all rows, because data in PostgreSQL may have changed since rows
  were marked for deletion leading to inconsistent line numbers.

- Retain the where-filter when refetching/redrawing a row-list buffer.

- Implement tab-completion on column names when reading a WHERE filter in the minibuffer.

- New text button in the table-list buffer which displays the list of queries currently running in
  the PostgreSQL backend that we are connected to.


## [0.15] - 2024-10-06

- New `WHERE` filtering functionality in a row-list buffer. Pressing `W` prompts you for the content
  of a `WHERE` clause (in SQL syntax), and applies it as a filter to the current table. Type `W`
  again to update the WHERE filter, and enter an empty string (type `W RET`) to cancel the filter.

- New **multi-row delete** functionality: in a row-list buffer, press `d` to mark the current row
  for deletion. It will be displayed in red. You can select multiple rows, and unmark (deselect) a
  row by pressing `u`. Pressing `d` in a row-list buffer will delete the selected rows from
  PostgreSQL and from the current row-list.

- New function `pgmacs-funcall-cell` that allows the user to call a function on a cell value,
  without updating the database. This can be used to implement custom user-specified functions that
  operate on the database value at point (dictionary lookup, web search, elisp evaluation, etc.).
  The existing internal function `pgmacs--funcall-cell` is renamed to `pgmacs--setf-cell` and
  modified always to  update the database with the result of the function call.

- Modify keybindings in row-list buffers: `r` now redraws only the pgmacstbl, without refetching
  data from PostgreSQL, and `g` refetches data from PostgreSQL and does a full redraw (as in the
  main table-list buffer).


## [0.14] - 2024-09-21

- Run the SchemaSpy Podman/Docker container with the `--userns=keep-id` commandline option in order
  for generated files to be owned by the user running Emacs (when running in rootless mode).

- SchemaSpy SVG output is rewritten before display to inline the xlinked PNG files that mark foreign
  key columns. These external xlinked PNG files were not being displayed by the SVG support in
  Emacs. We inline a vector representation of the key icon encoded as an SVG data URL.

- On graphical SVG-capable displays, small SVG icons are displayed alongside the names of tables in
  the table-list buffer, and user names and database names in the header line.


## [0.13] - 2024-08-24

- Display JSONB columns in the same way as JSON columns.

- In a row-list buffer, display meta-information on any table indexes under the column
  metainformation.
  
- Multi-line content in columns is truncated, and only the first line shown (along with the default
  ellipsis returned by function `truncate-string-ellipsis`). To display the full content of a
  truncated cell in a dedicated buffer, type `v`.
  
- The text button `Display procedures` in the main table-list buffer displays a table containing all
  the user-defined functions and procedures in the current database.

- The header line now shows whether the current database connection is secured with TLS.

- New function `pgmacs-register-column-displayer` allows the user to register a dedicated display
  function for a particular column in a particular table. The display-function takes three
  arguments: the cell-value, max-width, table. This functionality can be used to display BYTEA
  columns as inline images, for example.

- The SchemaSpy functionality has been extended to allow the application to be run on the entire
  database, displaying a graphical representation of the relations between tables.


## [0.12] - 2024-08-06

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
