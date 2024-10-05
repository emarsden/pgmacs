# Running SQL queries

You can also view (but obviously not edit!) the output from an SQL query you enter. Type
<kbd>e</kbd> in a PGmacs buffer, which will prompt you for an SQL query in the minibuffer, then
display the output in a dedicated temporary buffer. Type <kbd>q</kbd> to kill the temporary buffer.

Likewise, PGmacs can display the output from an SQL query in an Emacs buffer. Edit your SQL query in
a dedicated buffer, that you set to `sql-mode` or your favorite alternative. In the main PGmacs
table-list buffer or a row-list buffer, type <kbd>E</kbd> and PGmacs will prompt for the buffer
name, then display the query output in a dedicated temporary buffer.

![Screenshot table](img/screenshot-sql-query.png)
