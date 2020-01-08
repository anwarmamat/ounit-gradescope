#!/usr/bin/env bash
eval $(opam env)

dune build test/public.exe

dune build gradescope/xml_parser.exe

