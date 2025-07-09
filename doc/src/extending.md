# Extending and customizing PGmacs

A big advantage of building on the Emacs platform is that PGmacs is automatically quite easy to
extend and customize. You can extend it using the same programming language (Emacs Lisp) and APIs
as those that are used to build PGmacs itself.

Here’s a simple example where we add a key binding that applies in row-list buffers to implement a
web search on the content of the current cell (where the cursor is located). This is easy to
implement using the function `pgmacs-funcall-cell`, which calls an Emacs Lisp function on the
content of the current cell.

We define a function that executes a web search, here using the EWW browser built into Emacs with
the “html” interface of the DuckDuckGo search engine. 

```lisp
(defun my/ddg-query (q)
  (eww-browse-url (format "http://ddg.gg/html/?%s"
                          (url-build-query-string `(("q" ,q))))))
```

Then we define a function that calls our ddg-query function on the current cell contents, and define
a keybinding for it in the `pgmacs-row-list-map/table` keymap which is active when point is in the
tabular data in a row-list buffer.

```lisp
(require 'pgmacs)

(defun my/pgmacs-ddg-cell ()
  (interactive)
  (pgmacs-funcall-cell #'my/ddg-query))

(define-key pgmacs-row-list-map/table (kbd "D") #'my/pgmacs-ddg-cell)
```

This is what the functionality looks like once you’ve included this code in your Emacs
initialization file: 


<video width="80%" autoplay loop>
  <source
  src="https://github.com/emarsden/emarsden.github.io/raw/refs/heads/main/assets/pgmacs-extend-ddg.mp4">
</video>
