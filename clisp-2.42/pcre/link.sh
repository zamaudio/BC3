files='cpcre.o'
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files} -lpcre"
NEW_MODULES='pcre'
TO_LOAD='pcre'
TO_PRELOAD="preload.lisp"
