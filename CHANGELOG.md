# Changelog


## [0.2] - Unreleased

- `e` in keymap reads an SQL query from the minibuffer and displays the output in a temporary buffer.

- Pressing `k` in a table view copies the current row to a special kill ring. Pressing `y` then
  pastes the copied row into the table, taking care to use defaults for any colums for which a
  schema-specified default value is defined.

- In table buffers, include a button that dumps the table as CSV to an Emacs buffer.

- Faces `pgmacs-table-header` and `pgmacs-table-data` are used to display the header and the rows of
  database tables.
  
- Variable `pgmacs-row-colors` specifies the colors used for alternating rows in a database table.

- Variable `pgmacs-row-limit` specifies the maximum number of rows to retrieve per database query,
  before results are paginated.
