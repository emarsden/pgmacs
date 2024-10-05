# Displaying a database table: the row-list buffer

A **row-list buffer** shows some metainformation on the table and its columns, then the row data in
tabular form, as illustrated below.

![Screenshot table](img/screenshot-table.png)

If the table contains a large number of rows, the contents will be **paginated**, with `Next` and
`Previous` buttons to move page by page. The number of rows in each page is determined by the
variable `pgmacs-row-limit`.

A column which is a part of a primary key will be displayed using the customizable
`pgmacs-column-primary-key` face, which defaults to a bold version of the `pgmacs-table-data` face.
A column which references a foreign key will be displayed using the `pgmacs-column-foreign-key`
face, which defaults to a blue color.

The following keys are bound when the point is located in the row-list table:

| Key                            | Binding                                                                              |
|--------------------------------|--------------------------------------------------------------------------------------|
| <kbd>v</kbd>                   | Display the value at point in a dedicated buffer.                                    |
| <kbd>RET</kbd>                 | Edit the value at point in the minibuffer, or jump to foreign table.                 |
| <kbd>w</kbd>                   | Edit the value at point in a widget-based buffer.                                    |
| <kbd>!</kbd>                   | Run a shell command on the value at point, replacing the output if prefix argument.  |
| <kbd>Alt</kbd>-<kbd>u</kbd>    | Upcase the content of the current cell and update PostgreSQL.                        |
| <kbd>Alt</kbd>-<kbd>l</kbd>    | Downcase the content of the current cell and update PostgreSQL.                      |
| <kbd>Alt</kbd>-<kbd>c</kbd>    | Capitalize the content of the current cell and update PostgreSQL.                    |
| <kbd>DEL</kbd>                 | Delete the row at point.                                                             |
| <kbd>Alt</kbd>-<kbd>left</kbd> | Move to the previous column.                                                         |
| <kbd>M</kbd>-<kbd>right</kbd>  | Move to the next column.                                                             |
| <kbd>o</kbd>                   | Prompt for a table name and open a new buffer displaying that table's data.          |
| <kbd>+</kbd>                   | Insert a new row into the current table, prompting for new values in the minibuffer. |
| <kbd>i</kbd>                   | Insert a new row, prompting for new values in a dedicated buffer.                    |
| <kbd>k</kbd>                   | Copy the current row.                                                                |
| <kbd>y</kbd>                   | Paste (yank) the copied row.                                                         |
| <kbd>j</kbd>                   | Copy the current row to the kill ring in JSON format.                                |
| <kbd>R</kbd>                   | Rename the current column.                                                           |
| <kbd><</kbd>                   | Move point to the first row of data.                                                 |
| <kbd>></kbd>                   | Move point to the last row of data.                                                  |
| number                         | Move point to the nth column (numbering is zero-based).                              |
| <kbd>e</kbd>                   | Open a new buffer to display the result of an SQL query.                             |
| <kbd>E</kbd>                   | Execute SQL from an Emacs buffer and display the output.                             |
| <kbd>r</kbd>                   | Redraw the table (does not refetch data from PostgreSQL).                            |
| <kbd>n</kbd>                   | Next page of output (if table contents are paginated).                               |
| <kbd>p</kbd>                   | Previous page of output (if table contents are paginated).                           |
| <kbd>S</kbd>                   | Sort the table by the current column.                                                |
| <kbd>{</kbd>                   | Make the current column narrower.                                                    |
| <kbd>}</kbd>                   | Make the current column wider.                                                       |
| <kbd>T</kbd>                   | Jump to the main PGmacs table-list buffer.                                           |
| <kbd>q</kbd>                   | Kill the current buffer.                                                             |



## Follow foreign key references

A column that references data in a foreign table (`FOREIGN KEY`) will be shown in blue. If you type
<kbd>RET</kbd> when point is located on a foreign key reference, PGmacs will jump to the referenced
row and column in the other table. A new row-list buffer is opened; type <kbd>q</kbd> to come back
to the original row-list buffer.

![Following foreign key](img/follow-foreign-key.gif)


