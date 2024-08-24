# Customizing display of specific table columns


The function `pgmacs-register-column-displayer` allows you to register a dedicated display function
for a specific column in a particular table. It takes the following arguments:

- a table name (potentially schema-qualified)

- a column name

- a display function, which takes three arguments: the cell value, the cell max-width and the
  current table.

As an illustration, below we register a column display function for a column that contains images
stored in the database as `BYTEA`. The display function displays the images inline in the row-list.

Suppose we have the following SQL schema: 

```sql
CREATE TABLE inlineimg(
  id SERIAL PRIMARY KEY,
  image BYTEA,
  created TIMESTAMP DEFAULT now())
```

Here is some sample Emacs Lisp code to insert some example rows in the table, using the
[pg-el](https://github.com/emarsden/pg-el/) library:

```lisp
(dotimes (i 20)
   (let* ((img (with-temp-buffer
                 (url-insert-file-contents "https://picsum.photos/120/40")
                    (buffer-string)))
          (res (pg-exec-prepared con "INSERT INTO inlineimg(image) VALUES($1)"
                                 `((,img . "bytea")))))
     (message "Inserted image: %s" (pg-result res :status))))
```

and here is how to defined a custom display function that displays each image inline: 

```lisp
(defun my-inline-image-displayer (value _max-width _table)
  (let* ((img (create-image value nil t))
         (txt (propertize " " 'display img 'rear-nonsticky t)))
    (or txt "<invalid img>")))

(pgmacs-register-column-displayer "inlineimg" "image" #'my-inline-image-displayer)

```

and finally, this is what the row-list buffer for that table looks once the display function has
been registered: 

![Screenshot of customized column display](img/screenshot-customize-column-display.png)


