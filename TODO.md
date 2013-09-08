TODO
----


## Code

* switch to 32-bit or unbounded int?
* allow accessing the data as a storable vector (instead of unboxed?)
  This would allow direct use as a Ptr into applications that need pixmaps.

## Tests

* create more test files using tools from netpbm
* create 16-bit tests with convert and gimp
* create some more 16-bit tests by using `pamdepth`


## Documentation

* collect all trade-off and spec-diversions (especially trailing garbage and Int decisions)
