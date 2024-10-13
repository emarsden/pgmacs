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

| Key                             | Binding                                                                              |
|---------------------------------|--------------------------------------------------------------------------------------|
| <kbd>v</kbd>                    | Display the value at point in a dedicated buffer.                                    |
| <kbd>RET</kbd>                  | Edit the value at point in the minibuffer, or jump to foreign table.                 |
| <kbd>w</kbd>                    | Edit the value at point in a widget-based buffer.                                    |
| <kbd>!</kbd>                    | Run a shell command on the value at point, replacing the output if prefix argument.  |
| <kbd>&</kbd>                    | Run a program asynchronously with the value at point as first argument.              |
| <kbd>Alt</kbd>-<kbd>u</kbd>     | Upcase the content of the current cell and update PostgreSQL.                        |
| <kbd>Alt</kbd>-<kbd>l</kbd>     | Downcase the content of the current cell and update PostgreSQL.                      |
| <kbd>Alt</kbd>-<kbd>c</kbd>     | Capitalize the content of the current cell and update PostgreSQL.                    |
| <kbd>DEL</kbd>                  | Delete the row at point.                                                             |
| <kbd>Alt</kbd>-<kbd>left</kbd>  | Move to the previous column.                                                         |
| <kbd>Alt</kbd>-<kbd>right</kbd> | Move to the next column.                                                             |
| <kbd>W</kbd>                    | Apply an SQL WHERE filter to the rows displayed.                                     |
| <kbd>o</kbd>                    | Prompt for a table name and open a new buffer displaying that table's data.          |
| <kbd>+</kbd>                    | Insert a new row into the current table, prompting for new values in the minibuffer. |
| <kbd>i</kbd>                    | Insert a new row, prompting for new values in a dedicated buffer.                    |
| <kbd>k</kbd>                    | Copy the current row.                                                                |
| <kbd>y</kbd>                    | Paste (yank) the copied row.                                                         |
| <kbd>j</kbd>                    | Copy the current row to the kill ring in JSON format.                                |
| <kbd>d</kbd>                    | Mark the current row for deletion.                                                   |
| <kbd>u</kbd>                    | Unmark the current row (deselect it for deletion).                                   |
| <kbd>U</kbd>                    | Unmark all rows (deselect them for deletion).                                        |
| <kbd>x</kbd>                    | Delete marked rows.                                                                  |
| <kbd>R</kbd>                    | Rename the current column.                                                           |
| <kbd><</kbd>                    | Move point to the first row of data.                                                 |
| <kbd>></kbd>                    | Move point to the last row of data.                                                  |
| number                          | Move point to the nth column (numbering is zero-based).                              |
| <kbd>e</kbd>                    | Open a new buffer to display the result of an SQL query.                             |
| <kbd>E</kbd>                    | Execute SQL from an Emacs buffer and display the output.                             |
| <kbd>r</kbd>                    | Redraw the table (does not refetch data from PostgreSQL).                            |
| <kbd>n</kbd>                    | Next page of output (if table contents are paginated).                               |
| <kbd>p</kbd>                    | Previous page of output (if table contents are paginated).                           |
| <kbd>S</kbd>                    | Sort the table by the current column.                                                |
| <kbd>{</kbd>                    | Make the current column narrower.                                                    |
| <kbd>}</kbd>                    | Make the current column wider.                                                       |
| <kbd>T</kbd>                    | Jump to the main PGmacs table-list buffer.                                           |
| <kbd>q</kbd>                    | Bury the current buffer.                                                             |



## Run a shell command or an external application on cell value

There are two methods for running an external (non-Emacs) command on the current cell value:

- Run a Unix filter shell command with the cell value as input: press <kbd>!</kbd>. This works
  similarly to the standard Emacs `shell-command` command, which is bound to
  <kbd>M</kbd>-<kbd>!</kbd>. If called with a prefix argument, it will update the database value to
  the result of the shell command.

- Run a program asynchronously with the cell value as its first commandline argument: press
  <kbd>&amp;</kbd>. This works similarly to the standard Emacs `async-shell-command`, which is bound
  to <kbd>M</kbd>-<kbd>&amp;</kbd>.

For example, to count the number of characters in the current cell, type

    ! wc -c

To downcase the value of a text cell (and modify the value in the database) use

    C-u ! tr '[:upper:]' '[:lower]'

To reverse the order of the characters in the cell (and modify the value in
the database), use

    C-u ! rev

On a Microsoft Windows machine, you may need to install the [MSYS2](https://www.msys2.org/)
commandline tools for these examples to work.

If the current cell contains a file name, you can launch your system’s default application for that
filename extension by typing <kbd>&amp;</kbd> then entering `xdg-open` (or `open` on a MacOS
machine).



## Follow foreign key references

A column that references data in a foreign table (`FOREIGN KEY`) will be shown in blue. If you type
<kbd>RET</kbd> when point is located on a foreign key reference, PGmacs will jump to the referenced
row and column in the other table. A new row-list buffer is opened; type <kbd>q</kbd> to come back
to the original row-list buffer.

![Following foreign key](img/follow-foreign-key.gif)



## Filtering rows displayed using a WHERE expression

You may wish only to display the rows in a table that match an SQL `WHERE` filter. If you press
<kbd>W</kbd> in a row-list buffer, PGmacs will prompt you for a WHERE expression to use as a filter.
In the example illustrated by the video below, table `temperatures` includes a column
`measurement`. If you type a WHERE filter of `measurement < -5`, PGmacs will display rows
corresponding to the following SQL query:

```sql
SELECT * FROM temperatures WHERE measurement < -5
```

You can type <kbd>W</kbd> again to enter a new filter. To cancel the filter, type <kbd>W</kbd> and enter an
empty string (enter <kbd>W</kbd> <kbd>RET</kbd>).

The screen recording below illustrates the use of a WHERE filter on a large table that contains
various attributes concerning French inhabited locations. It starts by using a WHERE filter to
display only the row corresponding to the city of Toulouse based on its zip code, copies that row as
JSON with <kbd>j</kbd>, then with the copied latitude and longitude prepares a PostGIS query to
select (with an updated WHERE filter) rows that are within a certain geographic distance of
Toulouse, using the PostGIS function `ST_DistanceSphere`.

![WHERE filter using a PostGIS query](https://github.com/emarsden/emarsden.github.io/blob/main/assets/pgmacs-where-filter-postgis.gif)


## Dired-like multi-row delete

You can “mark” rows for later deletion by pressing <kbd>d</kbd>. Each row marked for deletion will
be highlighted in a red color. You can then delete the marked rows by pressing <kbd>x</kbd> (for
“expunge”, as in `dired-mode`). To unmark a row, press <kbd>u</kbd>.


The screen capture below illustrates this functionality in
operation, combined with the WHERE filtering described above.


<video width="80%" autoplay loop>
  <source
  src="https://github.com/emarsden/emarsden.github.io/raw/refs/heads/main/assets/pgmacs-where-filter-multidelete.mp4">
</video>

