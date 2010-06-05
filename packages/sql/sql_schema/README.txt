---+ sql/sql_schema - Bio DB Schema Repository

This directory contains a collection of schemas for major biological databases. The schemas are in prolog syntax and can be loaded with load_schema_defs/1.

* 

---++ Regenerating

The schemas can be regenerated using:

==
blip-sql sql-metadata -r enscore/homo_sapiens_core
==

However, the unique constraints must be added manually (these can be omitted, but queries may be slower - unique constraints are used by the prolog query optimizer)

@see ../odbc_setup.txt
