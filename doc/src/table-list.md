# The table list buffer

The table list buffer is the main PGmacs buffer. It shows some metainformation concerning the
PostgreSQL backend that you are connected to (version, database size on disk, etc.), followed by a
tabulated list of all the tables in the database (at least the tables which are visible to your
current PostgreSQL user).

![Screenshot table list](img/screenshot-overview.png)


The following keys are bound when the point is located in the table list buffer: 

| Key            | Binding                                                                              |
|----------------|--------------------------------------------------------------------------------------|
| <kbd>RET</kbd> | Open a new buffer to browse/edit the table at point.                                 |
| <kbd>Del</kbd> | Delete the table at point.                                                           |
| <kbd>R</kbd>   | Rename the table at point.                                                           |
| <kbd>o</kbd>   | Prompt for a table to browse/edit in a new buffer.                                   |
| <kbd>p</kbd>   | New buffer listing the functions and procedures in the current database.             |
| <kbd>e</kbd>   | Open a new buffer to display the output from an SQL query.                           |
| <kbd>E</kbd>   | Execute SQL from an Emacs buffer and display the output.                             |
| <kbd>S</kbd>   | [Run SchemaSpy](schemaspy.html) on the current database and display the SVG output.   |
| <kbd><</kbd>   | Move to the beginning of the table list.                                             |
| <kbd>></kbd>   | Move to the end of the table list.                                                   |
| <kbd>{</kbd>   | Make the current column narrower.                                                    |
| <kbd>}</kbd>   | Make the current column wider.                                                       |
| <kbd>g</kbd>   | Redraw the current buffer (refetches data from PostgreSQL).                          |
| <kbd>h</kbd>   | Display help for the table-list buffer.                                              |
| <kbd>q</kbd>   | Bury the current buffer.                                                             |




The `More backend information` button will open a buffer that displays further information
concerning the PostgreSQL backend that you are connected to, including the list of available
extensions.

![Screenshot backend information](img/backend-information-buffer.png)



## Custom buttons in the table-list buffer

The list of buttons displayed above the list of tables (“Display tables", “More backend information” and so on)
is user-customizable via the variable `pgmacs-table-list-buttons`. To add a new button to this list,
add code such as the following to your Emacs initialization file:

```lisp
(require 'pgmacs)

(add-to-list 'pgmacs-table-list-buttons
   (pgmacs-shortcut-button
     :label "The displayed label"
     :action #'my/function-of-zero-arguments
     :help-echo "Help text echoed to minibuffer"))
```

If you prefer the button to be added to the end of the list, add a last argument of `t` to the
`add-to-list` invocation.
