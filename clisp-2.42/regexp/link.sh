file_list='regexi.o'
mod_list='regexp'
if test -z "regex.o"; then
  rm -f regex*; # use system-wide regex implementation
fi
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list regex.o  regexp.dvi"
NEW_LIBS="$file_list regex.o "
NEW_MODULES="$mod_list"
TO_LOAD='regexp'
TO_PRELOAD='preload.lisp'
