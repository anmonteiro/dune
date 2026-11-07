
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <errno.h>

#ifndef _WIN32

#include <caml/signals.h>

#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
typedef struct dirent directory_entry;

static int is_dot_or_dot_dot(const char *name)
{
  return name[0] == '.'
    && (name[1] == '\0' || (name[1] == '.' && name[2] == '\0'));
}

value val_file_type(int typ) {
  switch(typ)
    {
#ifndef __HAIKU__     
   case DT_REG:
      return Val_int(0);
    case DT_DIR:
      return Val_int(1);
    case DT_CHR:
      return Val_int(2);
    case DT_BLK:
      return Val_int(3);
    case DT_LNK:
      return Val_int(4);
    case DT_FIFO:
      return Val_int(5);
    case DT_SOCK:
      return Val_int(6);
    case DT_UNKNOWN:
      return Val_int(7);
#endif
    default:
      return Val_int(7);
    }
}

CAMLprim value caml__dune_filesystem_stubs__readdir(value vd)
{
  CAMLparam1(vd);
  CAMLlocal2(v_filename, v_tuple);

  DIR * d;
  directory_entry * e;
  d = DIR_Val(vd);
  if (d == (DIR *) NULL) unix_error(EBADF, "readdir", Nothing);
  while (1) {
    caml_enter_blocking_section();
    errno = 0;
    e = readdir((DIR *) d);
    caml_leave_blocking_section();
    if (e == (directory_entry *) NULL) {
      if(errno == 0) {
        CAMLreturn(Val_int(0));
      } else {
        uerror("readdir", Nothing);
      }
    }
    if (!is_dot_or_dot_dot(e->d_name)) break;
  }
  v_filename = caml_copy_string(e->d_name);
  v_tuple = caml_alloc_small(2, 0);
  Field(v_tuple, 0) = v_filename;
#ifndef __HAIKU__
  Field(v_tuple, 1) = val_file_type(e->d_type);
#else
  Field(v_tuple, 1) = Val_int(7);
#endif
  CAMLreturn(v_tuple);
}

CAMLprim value caml__dune_filesystem_stubs__readdir_batch(value vd, value acc)
{
  CAMLparam2(vd, acc);
  CAMLlocal3(v_name, v_entry, v_pending);
  struct { size_t offset; value kind; } entries[64];
  char names[4096];
  size_t count = 0, used = 0;
  const char *pending = NULL;
  value pending_kind = Val_int(7);
  /* Readdir_batch: Continue = 0, End_of_directory = 1, Unknown = 2. */
  int tag = 0, error = 0;
  DIR *d = DIR_Val(vd);
  if (d == NULL) unix_error(EBADF, "readdir", Nothing);

  caml_enter_blocking_section();
  while (count < 64) {
    errno = 0;
    directory_entry *e = readdir(d);
    if (e == NULL) {
      error = errno;
      tag = 1;
      break;
    }
    if (is_dot_or_dot_dot(e->d_name)) continue;
#ifndef __HAIKU__
    value kind = val_file_type(e->d_type);
#else
    value kind = Val_int(7);
#endif
    size_t length = strlen(e->d_name) + 1;
    if (kind == Val_int(7) || length > sizeof(names) - used) {
      pending = e->d_name;
      pending_kind = kind;
      if (kind == Val_int(7)) tag = 2;
      break;
    }
    entries[count].offset = used;
    entries[count].kind = kind;
    memcpy(names + used, e->d_name, length);
    used += length;
    count++;
  }
  caml_leave_blocking_section();
  if (error != 0) unix_error(error, "readdir", Nothing);

  /* The last dirent stays valid: no further readdir occurs in this call. */
  if (pending != NULL) v_pending = caml_copy_string(pending);
  size_t total = count + (pending != NULL && tag != 2);
  for (size_t i = 0; i < total; i++) {
    value kind;
    if (i < count) {
      v_name = caml_copy_string(names + entries[i].offset);
      kind = entries[i].kind;
    } else {
      v_name = v_pending;
      kind = pending_kind;
    }
    v_entry = caml_alloc_small(2, 0);
    Field(v_entry, 0) = v_name;
    Field(v_entry, 1) = kind;
    v_name = caml_alloc_small(2, 0);
    Field(v_name, 0) = v_entry;
    Field(v_name, 1) = acc;
    acc = v_name;
  }
  if (tag == 2) {
    v_name = caml_alloc_small(2, tag);
    Field(v_name, 0) = v_pending;
    Field(v_name, 1) = acc;
  } else {
    v_name = caml_alloc_small(1, tag);
    Field(v_name, 0) = acc;
  }
  CAMLreturn(v_name);
}

#else
CAMLprim value caml__dune_filesystem_stubs__readdir(value vd)
{
  unix_error(ENOSYS, "readdir", Nothing);
}

CAMLprim value caml__dune_filesystem_stubs__readdir_batch(value vd, value acc)
{
  unix_error(ENOSYS, "readdir", Nothing);
}
#endif
