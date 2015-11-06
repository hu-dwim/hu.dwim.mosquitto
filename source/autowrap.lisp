(defpackage :hu.dwim.mosquitto.ffi
  (:use))

(in-package :hu.dwim.mosquitto.ffi)

(autowrap:c-include '(hu.dwim.mosquitto autospec "mosquitto-spec.h")
  :spec-path '(hu.dwim.mosquitto autospec)
  :symbol-regex (("(?i)^mosquitto_(.*)"
                  ()
                  "\\1"))
  :sysincludes '(
                 ;; if this is not here, then memset doesn't show up in the spec (maybe because stddef.h is reported to be missing without it?)
                 "/usr/include/linux/"
                 ;; TODO FIXME find it with an API; it's probably a c2ffi deficiency
                 "/media/store/work/llvm-3.6/lib/clang/3.6.2/include/"
                 ;; TODO FIXME delme
                 "/home/alendvai/workspace/mosquitto/lib")
  :exclude-arch ("i386-unknown-freebsd"
                 "x86_64-unknown-freebsd"
                 "i686-apple-darwin9"
                 "x86_64-apple-darwin9"
                 "i686-pc-windows-msvc"
                 "x86_64-pc-windows-msvc")
  :exclude-sources ("stdio.h"
                    "fcntl.h"
                    "unistd.h"
                    "libio.h"
                    "string.h"
                    "stdlib.h"
                    "types.h"
                    "time.h"
                    "locale.h"
                    "socket.h"
                    "sys/ioctl.h"
                    "sys/select.h")
  :include-definitions ()
  :exclude-definitions ("^_"
                        "^va_list$")
  :no-accessors t)
