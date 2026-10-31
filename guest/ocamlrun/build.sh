#!/bin/sh
# Cross-compile OCaml's bytecode interpreter (ocamlrun) for riscv64 Linux,
# statically linked against glibc, without running OCaml's configure.
#
#   guest/ocamlrun/build.sh OCAML_SRC OUT
#
# OCAML_SRC must be the same version as the host compiler (so host-compiled
# bytecode is compatible). The generated configuration headers come from
# the host's OCaml installation: m.h/s.h describe an LP64, little-endian,
# glibc Linux target, which riscv64 is too. Only zstd (no riscv64 libzstd)
# is switched off.
set -eu

src=$(cd "$1" && pwd)
out=$2
cc=${CROSS_CC:-riscv64-suse-linux-gcc}
host_caml=$(ocamlfind ocamlc -where 2>/dev/null || ocamlc -where)/caml
build=$(mktemp -d)
trap 'rm -rf "$build"' EXIT

# 1. Configuration headers
mkdir -p "$build/caml"
for h in m.h version.h exec.h opnames.h jumptbl.h; do cp "$host_caml/$h" "$build/caml/$h"; done
grep -v 'HAS_ZSTD' "$host_caml/s.h" > "$build/caml/s.h"
# runtime/caml/*.h include these by bare name, so they must also be found
# directly on the include path
for h in m.h s.h version.h exec.h opnames.h jumptbl.h; do cp "$build/caml/$h" "$build/$h"; done
cat > "$build/build_config.h" <<'H'
#define OCAML_STDLIB_DIR "/usr/lib/ocaml"
#define HOST "riscv64-unknown-linux-gnu"
H

# 2. Sources: runtime_COMMON_C_SOURCES + runtime_BYTECODE_ONLY_C_SOURCES
common="addrmap afl alloc array backtrace bigarray blake2 callback codefrag compare
custom debugger domain dynlink extern fail fiber finalise floats gc_ctrl gc_stats
globroots hash intern ints io lexing lf_skiplist main major_gc md5 memory memprof
meta minor_gc misc obj parsing platform printexc prng roots runtime_events
shared_heap signals skiplist startup_aux str sync sys unix weak"
bytecode_only="backtrace_byt fail_byt fix_code interp startup_byt zstd"
sources=""
for f in $common $bytecode_only; do sources="$sources $src/runtime/$f.c"; done

# 3. prims.c: the table of C primitives, as runtime/gen_primsc.sh builds it
LC_ALL=C sed -n 's/^CAMLprim value \([a-z][a-z0-9_]*\).*$/\1/p' $sources | sort | uniq > "$build/primitives"
{
  echo '#define CAML_INTERNALS'
  echo '#include "caml/mlvalues.h"'
  echo '#include "caml/prims.h"'
  sed -n -e '/^CAMLprim value .*)/p' -e '/^CAMLprim value [^)]*$/,/)/p' $sources |
    sed -e 's/^CAMLprim /CAMLextern /' -e 's/).*$/);/'
  echo 'const c_primitive caml_builtin_cprim[] = {'
  sed -e 's/.*/  (c_primitive) &,/' "$build/primitives"
  echo '  0 };'
  echo 'const char * const caml_names_of_builtin_cprim[] = {'
  sed -e 's/.*/  "&",/' "$build/primitives"
  echo '  0 };'
} > "$build/prims.c"

# 4. Compile and link
"$cc" -O2 -fno-strict-aliasing -fwrapv -pthread -D_FILE_OFFSET_BITS=64 \
  -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME \
  -I "$build" -I "$src/runtime" \
  $sources "$build/prims.c" \
  -static -lm -o "$out"
echo "built $out ($(wc -l < "$build/primitives") primitives)"
