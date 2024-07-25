# The table list buffer

The table list buffer is the main PGmacs buffer. It shows some metainformation concerning the
PostgreSQL backend that you are connected to (version, database size on disk, etc.), followed by a
tabulated list of all the tables in the database (at least the tables which are visible to your
current PostgreSQL user).

![Screenshot table list](img/screenshot-overview.png)


The following keys are bound when the point is located in the table list buffer: 

| Key   | Binding                                                  |
|-------|----------------------------------------------------------|
| `RET` | Open a new buffer to browse/edit the table at point.     |
| `DEL` | Delete the table at point.                               |
| `r`   | Rename the table at point.                               |
| `o`   | Prompt for a table to browse/edit in a new buffer.       |
| `e`   | Open a new buffer to display the result of an SQL query. |
| `<`   | Move to the beginning of the table list.                 |
| `>`   | Move to the end of the table list.                       |
| `{`   | Make the current column narrower.                        |
| `}`   | Make the current column wider.                           |
| `g`   | Redraw the current buffer.                               |
| `h`   | Display help for the table-list buffer.                  |
| `q`   | Bury the current buffer.                                 |


The `More backend information` button will open a buffer that displays further information
concerning the PostgreSQL backend that you are connected to, including the list of available
extensions.

![Screenshot backend information](img/backend-information-buffer.png)
