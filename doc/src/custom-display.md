# Customizing display of specific table columns


The function `pgmacs-register-column-displayer` allows you to register a dedicated display function
for a specific column in a particular table. It takes the following arguments:

- a table name (potentially schema-qualified)

- a column name

- a display function, which takes three arguments: the cell value, the cell max-width and the
  current table.


## Illustration: display BYTEA images inline

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



## Illustration: highlight anomalous values in a column

A second illustration of the use of this function to highlight values in a column that are higher
than a particular threshold by displaying them in a particular color. Suppose we have the following
SQL schema:

```sql
CREATE TABLE temperatures(
  id SERIAL PRIMARY KEY,
  measurement FLOAT,
  created TIMESTAMP DEFAULT now())
```

Some Emacs Lisp code to insert fake measurement data in the table, using the
[pg-el](https://github.com/emarsden/pg-el/) library:

```lisp
(dotimes (i 500)
   (let* ((rnd (* 0.5 (1+ (/ (random) (float most-positive-fixnum)))))
          (temp (+ -8 (* rnd 50))))
      (pg-exec-prepared con "INSERT INTO temperatures(measurement) VALUES ($1)"
                        `((,temp . "float")))))))
```

A custom display function that sets the foreground color for values above 40 to dark 

```lisp
(defun my-highlight-anomaly (value _max-width _table)
  (if (> (string-to-number value) 40.0)
      (propertize value 'face '(:foreground "darkred"))
    value))

(pgmacs-register-column-displayer "temperatures" "measurement" #'my-highlight-anomaly)
```


