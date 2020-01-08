#!/usr/bin/env bash

_build/default/test/public.exe  -display=false -output-junit-file=log2.xml

_build/default/gradescope/xml_parser.exe




