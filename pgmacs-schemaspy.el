;;; pgmacs-schemaspy.el --- Support for the SchemaSpy tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>


;; To run SchemaSpy on the current database or on the current table, and display images describing
;; the schema. The default setting runs SchemaSpy in a Docker/Podman software container, using
;; Podman or Docker. This is the easiest way of running SchemaSpy, because all necessary
;; dependencies are preinstalled. Alternatively (see commented commandline), you can run the
;; SchemaSpy java application natively, which requires the following software to be installed:
;;
;;  - SchemaSpy (see schemaspy.org)
;;  - Java
;;  - GraphViz
;;  - JDBC support for PostgreSQL (here in /usr/share/java/postgresql-jdbc4.jar, installable for example
;;    using "sudo apt install libpostgresql-jdbc-java")
;;
;; The commandline below with "--userns=keep-id" is suitable for running a container in rootless
;; mode with Podman. This is because the SchemaSpy Dockerfile creates a new user "java" then runs as
;; that user. We can't map that new user id to our own id, because the java uid is not known prior
;; to the container running. With the --userns=keep-id option however, the USER will be mapped to
;; our own uid when running rootless.
(defcustom pgmacs-schemaspy-cmdline
  "podman run -v %D:/output --userns=keep-id --network=host docker.io/schemaspy/schemaspy:latest -t pgsql11 -host %h -port %P -u %u -p %p -db %d -imageformat svg"
  "Commandline for running the SchemaSpy application or container.

This commandline will be used both for generating an image
representing the relations between tables in the current
database, and for generating an image representing the schema of
the current table. In the latter case, the string ` -s %s -i %t'
will be appended to the commandline (with `%s' replaced by the
table schema name and `%t' by the table name).

SchemaSpy can be run as a Java application installed on the local
machine, or (probably easier for most users) in a Docker/Podman
software container that contains the necessary dependencies.

In this commandline, %d is replaced by the database name, %h by
the hostname on which PostgreSQL is running, %P by the port it is
running on, %u by the user, %p by the password, %s by the current
table schema name, %t by the current table name and %D by the
directory (which will be created in the system temporary
directory) in which output files are created by SchemaSpy. The %s
and %t values will only be used when generating illustrations
concerning a specific table, rather than the entire database."
  :type 'string
  :group 'pgmacs)


(defun pgmacs--rewrite-schemaspy-svg (svg-pathname)
  "Clean up the SVG produced by SchemaSpy to improve display in Emacs.
The SVG produced by dot includes xlinked references to PNG files
that represent a key. These xlinked relative pathnames are not
displayed by the SVG support in Emacs. We replace them by an
inlined vector SVG image that is encoded as a data URI."
  (with-current-buffer (find-file-noselect svg-pathname t t)
    (cl-loop while (search-forward "xlink:href=\"../../images/primaryKeys.png\"" nil t)
             do (delete-region (match-beginning 0) (match-end 0))
             (insert "href=\"data:image/svg+xml,%3Csvg height='1362.976' viewBox='0 0 154.491 360.621' width='583.904' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M69.407 360.143c-8.545-1.912-12.989-5.943-15.73-14.271-1.263-3.84-1.716-7.444-3.181-25.306-2.999-36.562-4.773-64.722-6.027-95.69-.332-8.209-5.756-80.762-6.048-80.913-1.011-.52-8.86-5.98-10.696-7.44-3.498-2.783-9.747-9.042-12.344-12.364-6.935-8.871-11.455-18.535-13.708-29.314C.345 88.495-.015 84.632 0 76.886.026 63.563 2.333 53.394 7.8 42.49c3.93-7.834 8.006-13.364 14.96-20.295C35.21 9.785 49.463 2.732 66.707.446c5.045-.669 17.296-.567 22.225.184 11.299 1.723 21.418 5.533 30.339 11.423 8.456 5.583 16.442 13.447 22.31 21.97 11.561 16.793 15.58 38.777 11.155 61.031-3.871 19.471-14.575 35.045-32.054 46.637-2.62 1.738-4.966 3.545-5.216 4.016-1.007 1.905-3.055 7.729-4.826 13.73-1.734 5.874-4.422 16.976-4.422 18.264 0 .922-1.641 2.581-4.62 4.668-4.347 3.046-4.196 2.561-4.198 13.51-.001 10.612-.118 10.114 3.192 13.672 1.14 1.226 2.178 2.644 2.305 3.151.128.508.164 4.288.081 8.4-.122 6.066-.26 7.679-.73 8.537a35.426 35.426 0 0 0-.942 1.852c-.2.437-.534.794-.745.794-.646 0-4.22 3.592-4.22 4.241 0 .336-.25.818-.553 1.07-1.505 1.25-2.785 6.552-2.546 10.552.213 3.566.906 4.789 5.328 9.41 5.078 5.308 5.002 5.13 5.002 11.769 0 6.762-.117 7.03-5.466 12.523-2.079 2.134-4.041 4.389-4.36 5.01-.5.971-.58 2.548-.579 11.289.001 5.913.155 10.709.367 11.474.201.723.915 2.034 1.586 2.914.671.879 1.22 1.8 1.22 2.045s1.192 1.63 2.648 3.077c3.952 3.926 3.887 3.641 3.837 16.755-.047 12.092-.36 14.766-2.187 18.67-1.867 3.99-3.773 5.27-9.766 6.559-4.77 1.026-17.76 1.329-21.465.5z' fill='%23d6de00'/%3E%3Cpath d='M96.814 73.36a25.972 26.755 0 0 1-36.667-1.957 25.972 26.755 0 0 1 1.871-37.773 25.972 26.755 0 0 1 36.67 1.898 25.972 26.755 0 0 1-1.815 37.776' fill='%23fff'/%3E%3C/svg%3E\""))
    ;; drop the "Generated by SchemaSpy label" which is rather intrusive
    (goto-char (point-min))
    (when (search-forward "Generated by SchemaSpy</text>" nil t)
      (delete-region (line-beginning-position) (line-end-position)))
    (save-buffer)
    (kill-buffer)))

(defun pgmacs--schemaspy-table (&rest _ignore)
  "Run SchemaSpy on current table and display the SVG describing the schema."
  (interactive)
  (unless (display-graphic-p)
    (user-error "SchemaSpy will only work on a graphical terminal"))
  (unless (image-type-available-p 'svg)
    (user-error "SchemaSpy support needs SVG support in your Emacs"))
  (when (member (pgcon-server-variant pgmacs--con) '(spanner))
    (error "The Spanner database does not implement system tables needed by SchemaSpy"))
  (let* ((tmpdir (temporary-file-directory))
         (schemaspy-dir (expand-file-name "pgmacs-schemaspy" tmpdir)))
    (when (file-directory-p schemaspy-dir)
      (delete-directory schemaspy-dir t nil))
    (with-file-modes #o777
      (make-directory schemaspy-dir t))
    ;; The Docker image for schemaspy runs as user "java" for an obscure reason, so ensure that the
    ;; temporary schemaspy-dir is writable for all.
    (let ((ci (pgcon-connect-info pgmacs--con))
          (schema-name (if (pg-qualified-name-p pgmacs--table)
                           (pg-qualified-name-schema pgmacs--table)
                         "public"))
          (table-name (if (pg-qualified-name-p pgmacs--table)
                          (pg-qualified-name-name pgmacs--table)
                        pgmacs--table)))
      (when (eql :local (cl-first ci))
        (message "Replacing Unix connection by network connection to localhost for SchemaSpy"))
      (let* ((cmdline (concat pgmacs-schemaspy-cmdline
                              " -s %s -i %t"))
             (cmd (cl-multiple-value-bind (type host port dbname user password) ci
                    (let ((spec (list (cons ?h (if (eq type :local) "localhost" host))
                                      (cons ?P (or port 5432))
                                      (cons ?d dbname)
                                      (cons ?u user)
                                      (cons ?p password)
                                      (cons ?s schema-name)
                                      (cons ?t table-name)
                                      (cons ?D schemaspy-dir))))
                      (format-spec cmdline spec))))
             (out (format "%s/diagrams/tables/%s.1degree.svg"
                          schemaspy-dir
                          table-name)))
        (message "Running cmd %s, output to %s" cmd out)
        (shell-command cmd)
        (when (file-exists-p out)
          (pgmacs--rewrite-schemaspy-svg out)
          (find-file out))))))

;; We display only the "real relationships" summary SVG for the database; SchemaSpy generates many
;; other images including for each orphan table.
(defun pgmacs--schemaspy-database (&rest _ignore)
  "Run SchemaSpy on the current PGmacs database and display the SVG."
  (interactive)
  (unless (display-graphic-p)
    (user-error "SchemaSpy will only work on a graphical terminal"))
  (unless (image-type-available-p 'svg)
    (user-error "SchemaSpy support needs SVG support in your Emacs"))
  (let* ((tmpdir (temporary-file-directory))
         (schemaspy-dir (expand-file-name "pgmacs-schemaspy" tmpdir)))
    (when (file-directory-p schemaspy-dir)
      (delete-directory schemaspy-dir t))
    ;; The Docker image for schemaspy runs as user "java" for an obscure reason, so we need to
    ;; ensure that the temporary schemaspy-dir is writable for all.
    (with-file-modes #o777
      (make-directory schemaspy-dir t))
    (let ((ci (pgcon-connect-info pgmacs--con)))
      (when (eql :local (cl-first ci))
        (message "Replacing Unix connection by network connection to localhost for SchemaSpy"))
      (let ((cmd (cl-multiple-value-bind (type host port dbname user password) ci
                   (let ((spec (list (cons ?h (if (eq type :local) "localhost" host))
                                     (cons ?P (or port 5432))
                                     (cons ?d dbname)
                                     (cons ?u user)
                                     (cons ?p password)
                                     (cons ?D schemaspy-dir))))
                     (format-spec pgmacs-schemaspy-cmdline spec))))
            (out (format "%s/diagrams/summary/relationships.real.compact.svg" schemaspy-dir)))
        (message "Running cmd %s, output to %s" cmd out)
        (shell-command cmd)
        (when (file-exists-p out)
          (pgmacs--rewrite-schemaspy-svg out)
          (find-file out))))))

(provide 'pgmacs-schemaspy)

;; pgmacs-schemaspy.el ends here
