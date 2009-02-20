#!/bin/sh

sh add-index.sh main.html "<title>Lang-lisp project main page</title>"
sh add-index.sh about.html "<title>About/Contact</title>"
sh add-index.sh core-test.html "<title>Tests for lang-lisp</title>"
sh add-index.sh explanation.html "<title>Aspirations of lang-lisp</title>"
sh add-index.sh license.html "<title>Lang-lisp's license</title>"
sh add-index.sh stages.html "<title>Lang-lisp's development stages</title>"

cp top.htm contents.htm ..
