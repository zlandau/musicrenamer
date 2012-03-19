
Introduction
------------

This tool takes a directory of MP3s as an input and applies a set of rules to
change the naming and ID3 tags of the files. Using the `-e` option allows you
to add, remove or change rules before they're executed.  `getFileActions`
defines the rules that are applied to each action.

Note that the tool is a bit fragile, due to external tool-based parsing that
ID3.hs uses. Replacing this with a real ID3 library should fix this.
