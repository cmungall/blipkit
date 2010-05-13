---+ Connecting to and mapping to and from relational databases

---++ Abstract

The relational model can be viewed as a subset of prolog. This means that SQL and Prolog should integrate well together.

Use sql_compiler.pro, we can express SQL queries in Prolog syntax, and we can also perform more advanced mappings. See for example genome_sqlmap_enscore.pro

---++ Core Modules

  * sql_compiler.pro - translating between prolog and SQL queries
  * rdb_util.pro

---++ Package dependencies

This package is part of blip. See ../../../README.txt for details

Blip dependencies:

  * ../blipcore/README.txt

At some point this package may be cut loose from blipkit, as there is
nothing inherently biology-specific about it

Other dependencies:

 * odbc (from SWI-Prolog)

---++ Setup

See odbc_setup.txt -- most (but not all) of this package relies on database connectivity via the ODBC module in SWI-Prolog








