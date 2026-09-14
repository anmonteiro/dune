include_subdirs
---------------

The ``include_subdirs`` stanza is used to control how Dune considers
subdirectories of the current directory. The syntax is as follows:

.. code:: dune

     (include_subdirs <mode>)

Where ``<mode>`` maybe be one of:

- ``no``, the default
- ``unqualified``
- ``qualified``

.. important::
  It's not allowed for a subdirectory of a directory with
  ``(include_subdirs <x>)`` (where ``<x>`` is not ``no``) to contain one of the
  following stanzas:

    - ``library``
    - ``executable(s)``
    - ``test(s)``


``no`` (the default)
====================

When the ``include_subdirs`` stanza isn't present or ``<mode>`` is ``no``, Dune
considers subdirectories independent.


``unqualified``
===============

When ``<mode>`` is ``unqualified``, Dune will assume that the current
directory's subdirectories are part of the same group of directories. In
particular, Dune will simultaneously scan all these directories when looking
for OCaml/Reason files. This allows you to split
:doc:`/reference/dune/library`, :doc:`/reference/dune/executable` and
:doc:`/reference/dune/test` source files among several directories.

.. note::

  ``unqualified`` means that modules in subdirectories are seen as if they were
  all in the same directory. In particular, you cannot have two modules with
  the same name in two different directories.


``qualified``
===============

When ``<mode>`` is ``qualified``, subdirectories are part of the module
hierarchy. In the source tree, files in each subdirectory will be grouped into
submodules of the :doc:`/reference/dune/library`,
:doc:`/reference/dune/executable` or :doc:`/reference/dune/test` module group,
mirroring the directory structure.

Subdirectories are included recursively. However, recursion will stop when
encountering a subdirectory that contains another ``include_subdirs`` stanza.

.. tip::

   The :doc:`/reference/dune/ocamllex`, :doc:`/reference/dune/ocamlyacc` and
   :doc:`/reference/dune/menhir` stanzas must be defined in a ``dune`` file
   next to their corresponding source files, even when the directory group root
   is an ancestor.

Module group interfaces
^^^^^^^^^^^^^^^^^^^^^^^^

At each level of the source tree, Dune generates the module interface with
aliases for all its sub-modules with a capitalized name of the directory.

- In ``app.ml``, ``sub/other.ml`` is accessible at ``Sub.Other``:

.. code::

    dune
    app.ml
    sub
    └── other.ml


Group interfaces are configurable similarly to the
:doc:`/reference/dune/library` stanza "library interface".

- ``sub/sub.ml`` defines the module interface for the modules inside ``sub/``:

.. code::

    dune
    app.ml
    sub
    ├── sub.ml
    └── other.ml

.. warning::

   Currently :doc:`/reference/dune/menhir` stanzas may not be used as a module
   group interface due to a `Dune issue
   <https://github.com/ocaml/dune/issues/8989>`_.

Renaming Directories
^^^^^^^^^^^^^^^^^^^^

.. versionadded:: 3.25

The structured form of ``include_subdirs`` requires ``(lang dune 3.25)`` or later,
including when ``dirs`` is omitted. In ``qualified`` mode, it accepts a ``dirs``
field to choose module names independently of source directory names:

.. code:: dune

   (include_subdirs
    (mode qualified)
    (dirs (internal as public)))

Each ``(source as destination)`` entry maps a source directory path to a module
group path. In this example, ``internal/leaf.ml`` becomes ``Public.Leaf`` rather
than ``Internal.Leaf``. In a wrapped library named ``example``, clients refer to
it as ``Example.Public.Leaf``. The source files stay in ``internal/``; the
mapping does not rename or move files on disk.

The mapping also applies to descendants: ``internal/nested/leaf.ml`` becomes
``Public.Nested.Leaf``. To rename a nested directory as well, add a more specific
mapping:

.. code:: dune

   (include_subdirs
    (mode qualified)
    (dirs
     (internal as public)
     (internal/nested as public/exposed)))

Now ``internal/nested/leaf.ml`` becomes ``Public.Exposed.Leaf``. Mappings are
matched against the original source paths, and the longest matching source
prefix takes precedence. The destination specifies the complete replacement
path relative to the directory containing the stanza, including any renamed
parent directories.

The ``dirs`` field does not select which subdirectories are included. Directories
not covered by a mapping keep their usual module names. Omitting ``dirs`` is
equivalent to ``(include_subdirs qualified)``.

Group interface files keep the name of their source directory. With the mappings
above, ``internal/internal.ml`` defines ``Public``, and
``internal/nested/nested.ml`` defines ``Public.Exposed``. There is no need to
rename these files to ``public.ml`` or ``exposed.ml``. Fields that refer to
modules, such as a library's ``modules`` field, use the mapped names.

Directory mappings have the following restrictions:

- ``dirs`` is only allowed with ``(mode qualified)``.
- Source and destination paths are relative to the directory containing the
  stanza and must refer to descendants of that directory, not the directory
  itself or a parent.
- Source and destination paths must have the same number of components. A
  mapping cannot flatten the hierarchy or introduce an extra level.
- Destination components must form valid OCaml module names after capitalization.
  Mapped module paths must not conflict with another module or module group.
- Mappings for the same source directory must agree on the destination after
  variable expansion and path normalization. Repeating an identical mapping is
  allowed.
- Generated sources must not replace handwritten implementations or interfaces
  at the mapped module path. A generated implementation can still be paired
  with a handwritten interface, and vice versa.

Source and destination paths support :doc:`/concepts/variables`, for example
``(internal as %{read:../config/mapping})``. A mapping can read a generated file;
changes to that file update the module namespace on subsequent builds.

.. warning::

   Files read by a mapping must be outside the qualified directory group.
   Reading a source file or generated file inside the group creates a dependency
   cycle: Dune needs the mapping to determine the group's contents before it can
   read the file. Put mapping files in a separate directory outside the group,
   as in ``../config/mapping`` above.
