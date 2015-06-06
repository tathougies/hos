Hos Storage Layer
==================

The Hos storage server implements a dynamic relational federated database atop multiple, independent *storage servers*.

A dynamic relational database is one in which there is no set schema. Rather, the schema can only be determined by examining every object in the system. In practice, such an operation should never occur. In the Hos storage system, there is only one relation. Each entry in the relation has an arbitrary number of associated columns (called tags). Missing columns are assigned the values found in the tag descriptors metadata or are assigned the zero-length string.

Every data object is uniquely referenced internally by a reference to its underlying server plus a server-defined unique GUID.

Storage servers
----------------

Hos comes with two storage servers: hos-storage-foreign-fs and hos-storage-block. hos-storage-foreign-fs allows the use of standard foreign file systems to be used as hos storage backends, according to the Hos foreign file system storage standard. Hos-storage-block allows the direct use of a block device as a hos storage backend, using the HSS (Hos storage system) format.

The central storage server handles transactions and consistency across the entire database. Each storage server needs to provide one endpoint: com.hos.storage.item. It should support the Create, Update, Query, and Delete operations.


