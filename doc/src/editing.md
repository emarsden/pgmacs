# Editing data in a PostgreSQL table

Editing takes place in a [row-list buffer](row-list.md) that displays information from a particular
database table.


## Editing a column value

If your table has a primary key, you can edit the contents of the table. To modify a value, move the
cursor to the relevant column value and type `RET`. This will prompt you for the new value, and
immediately update the cell to the value you specified (it sends PostgreSQL an SQL command similar
to `UPDATE table_name SET column_name to X WHERE pk_col1 = value1 AND pk_col2 = value2`, where
`pk_col1` and `pk_col2` are the names of the columns that comprise the primary key constraint).
Please note that there is no undo support for editing operations; you'll have to re-execute an
update operation.

Note that PGmacs tells you the column type when prompting for the new value. You must specify a
value in the format accepted by PostgreSQL for that type (check the current value as displayed in the
minibuffer if you’re unsure of the expected format).

For certain column types and very long column values, it may be more convenient to use the
**widget-based editing interface** to edit a column value. Move the cursor to the relevant column value
and type `w`. This will open a dedicated buffer with an editing widget suitable for that column’s
type, as illustrated below for an HSTORE key->value map.

![Screenshot of hstore editing widget](img/screenshot-widget-hstore.png)

If you wish to abort editing, simply kill this editing buffer.


## Inserting, copying and deleting rows

To insert a new row into a table, press `+` in the row-list buffer. You will be prompted for the values
of each column for which a default value is not specified (in the minibuffer), then the new row will
be inserted. You can also insert a new row by entering new values in a widget-based buffer by
pressing `i` (this may be more convenient if the table contains many rows, or the values to enter
are very long).

To delete the row at point, press `<delete>` or `<backspace>` in a table buffer and confirm. Please
note that this deletes the current row in the PostgreSQL database, as well as in the Emacs buffer.
Deleting rows is only possible on tables that include a primary key constraint (this can be a single
column specified as `PRIMARY KEY`, or a constraint across a group of columns).

To copy/paste rows, press `k` to copy the row to the PGmacs kill buffer (this only copies, without
deleting the row), then `y` to insert a new row with the same values. Any columns that have a
default value specified (for example, primary key rows that pull a value from an integer sequence,
or are specified as `SERIAL`, or timestamp values that default to `now`) will be inserted with a new
generated value, rather than the value in the copied row.

All updates, insertions and deletions are immediately synced with the PostgreSQL server by sending it
the appropriate SQL `UPDATE TABLE`, `DELETE FROM` or `INSERT INTO` commands.



## Running a shell command on a cell value

Typing `!` in a row-list buffer runs a shell-command on the current cell value, and displays the
output in the echo area. The cell value is sent as standard input to the shell command, as for the
Emacs function `shell-command-on-region`. If called with a prefix argument, the current cell value
is replaced by the output of the cell command, and the PostgreSQL database is updated.

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

