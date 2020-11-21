#!/bin/bash
ocamlbuild -package batteries -no-hygiene m.native && time ./m.native
