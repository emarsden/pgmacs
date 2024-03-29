# Changelog

## [0.3] - Unreleased

- The comment on a table can be modified by pressing RET in the list-of-tables buffer.


## [0.2] - 2024-03-29

- New functions `pgmacs-open/string` to open PGMacs with a PostgreSQL connection string, and
  `pgmacs-open/uri` to open PGMacs with a PostgreSQL connection URI.
  
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
