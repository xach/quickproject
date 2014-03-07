# Quickproject

## Description

Quickproject creates the skeleton of a Common Lisp project.

For full documentation, see doc/index.html.

## Hacking

* Add the projet :

        CL-USER> (push #p"/projects/quickproject/" asdf:*central-registry*)
        CL-USER> (ql:quickload "quickproject")

* Run unit tests:

        CL-USER> (ql:quickload "quickproject-test")
		CL-USER> (lisp-unit:run-tests :all :quickproject-test)


## License

Quickproject is licensed under the MIT license; see LICENSE.txt for
details.

## Contact

For questions or comments, please email Zach Beane <xach@xach.com>.
