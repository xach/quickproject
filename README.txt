Quickproject creates the skeleton of a Common Lisp project by
automatically creating several files:

  * README.txt

  * <project>.asd

  * package.lisp (which defines a package named after the project)

  * <project>.lisp

For example, to create a new project called "screenscraper":

  (quickproject:make-project #p"src/screenscraper/" 
                             :depends-on '(cl-ppcre drakma closure-html))

The function interprets the last component of the pathname's directory
as the project name. You can override this interpretation by passing
an explicit :name argument. It then creates:

  * src/screenscraper/README.txt

  * src/screenscraper/screenscraper.asd

  * src/screenscraper/package.lisp

  * src/screenscraper/screenscraper.lisp

quickproject:*after-make-project-hooks* holds a list of functions that
are called with the same arguments as make-project after a project is
created. By default, the value is nil.

Quickproject is licensed under the MIT license; see LICENSE.txt for
details.
