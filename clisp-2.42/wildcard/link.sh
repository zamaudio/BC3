file_list=''
mod_list=''
if test -r wildcard.c; then
  file_list="$file_list"' wildcard.o'
  mod_list="$mod_list"' wildcard'
fi
if test -z "fnmatch.o"; then
  rm -f fnmatch.*; # use system-wide fnmatch implementation
fi
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list fnmatch.o wildcard.dvi"
NEW_LIBS="$file_list fnmatch.o"
NEW_MODULES="$mod_list"
TO_LOAD='wildcard'