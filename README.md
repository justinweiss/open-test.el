open-test
=========

Open-test is a hacked up emacs-lisp script that provides two main
features: 

1. From a Ruby file in a standard Ruby or Rails directory, such as
   `app/*` or `lib/`, enables one-click opening of an appropriately
   named test file.

2. From an appropriately named test file in the `test/*` directory,
   runs all test cases in the file in a compilation buffer.


Usage
-----

In your .emacs or init.el, include open-test's directory, and require
the library:

    (add-to-list 'load-path "~/path/to/open-test")
    (require 'open-test)

To open the corresponding test file from a Ruby buffer:
    (open-test)

To execute the test file in the current buffer:
    (run-test)

These can be bound to keys:
    (global-set-key [(f6)] 'run-test)


