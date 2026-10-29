#!/bin/bash
# build/make_runtime_mac.sh  --  Makes ovid-runtime-mac-<arch>: a portable R with every package build/renv.lock pins, that runs
# from any folder. "Open ovid.command" uses it when it sits beside it as runtime/.
#
#   bash build/make_runtime_mac.sh                      this Mac's arch, from the CRAN R installed in /Library/Frameworks
#   bash build/make_runtime_mac.sh --arch x86_64        Intel, from CRAN's x86_64 installer (downloaded, checked as notarized,
#                                                       unpacked, never installed); its packages are binaries from the
#                                                       snapshot in build/snapshot.txt. On Apple silicon it runs under
#                                                       Rosetta (arch -x86_64)
#   ... --install                                       also copies it to runtime/ beside "Open ovid.command"
#   ... --allow-newer                                   accept a package newer than the lockfile; each one is listed, and
#                                                       versions.txt records all
#
# How: R's own files are copied (no docs, tests or debug symbols); every compiled file's absolute R.framework paths are
# rewritten to @executable_path (so they resolve from wherever the folder is) and re-signed ad hoc; bin/R finds its home from
# where it is. The library holds R's base and recommended packages plus the lockfile's packages: copied from the installed R
# when that is the same arch and R version, else installed as binaries from the snapshot (build/runtime_packages.R, shared with the Windows build); a version that differs from the lockfile stops it.
# Output in build/out/ (not in git): the folder and a .tar.gz for a release.
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT="$(pwd)"
ARCH="$(uname -m)"; INSTALL=0; NEWER=0
while [ $# -gt 0 ]; do case "$1" in --arch) ARCH="$2"; shift 2;; --install) INSTALL=1; shift;; --allow-newer) NEWER=1; shift;; *) echo "unknown option $1" >&2; exit 1;; esac; done
[ "$ARCH" = arm64 ] || [ "$ARCH" = x86_64 ] || { echo "--arch is arm64 or x86_64" >&2; exit 1; }
RVER=$(python3 -c 'import json;print(json.load(open("build/renv.lock"))["R"]["Version"])')
NAME=ovid-runtime-mac-$ARCH
OUT="$ROOT/build/out/$NAME"; R="$OUT/R"
RUN=(); if [ "$ARCH" = x86_64 ] && [ "$(uname -m)" = arm64 ]; then RUN=(arch -x86_64); fi

# The R to copy: the installed framework when it is this arch and the lockfile's R version, else CRAN's installer unpacked.
FW=/Library/Frameworks/R.framework/Versions/Current/Resources
COPY_PKGS=0
if [ -x "$FW/bin/exec/R" ] && lipo -archs "$FW/bin/exec/R" | grep -qw "$ARCH" && \
   [ "$("$FW/bin/Rscript" -e 'cat(as.character(getRversion()))' 2>/dev/null)" = "$RVER" ]; then
  SRC="$(cd "$FW" && pwd -P)"; COPY_PKGS=1
else
  PKGDIR="$ROOT/build/out/cran"; mkdir -p "$PKGDIR"
  sub=$([ "$ARCH" = arm64 ] && echo sonoma-arm64 || echo big-sur-x86_64)
  pkg="$PKGDIR/R-$RVER-$ARCH.pkg"
  [ -f "$pkg" ] || { echo "Downloading CRAN's R $RVER for $ARCH"; curl -sSfL -o "$pkg" "https://cloud.r-project.org/bin/macosx/$sub/base/R-$RVER-$ARCH.pkg"; }
  pkgutil --check-signature "$pkg" | grep -q "trusted by the Apple notary service" || { echo "$pkg is not notarized by Apple" >&2; exit 1; }
  rm -rf "$PKGDIR/x-$ARCH"; pkgutil --expand-full "$pkg" "$PKGDIR/x-$ARCH"
  SRC="$(cd "$PKGDIR/x-$ARCH/R-fw.pkg/Payload/R.framework/Versions/Current/Resources" && pwd -P)"
fi

# every compiled file's R.framework paths to @executable_path, re-signed
relocate() {
  local n=0 f old rel id changed
  while IFS= read -r -d '' f; do
    file -b "$f" | grep -q 'Mach-O' || continue
    changed=0
    for old in $(otool -L "$f" | tail -n +2 | awk '{print $1}' | grep '^/Library/Frameworks/R.framework/' || true); do
      rel="${old#/Library/Frameworks/R.framework/Versions/*/Resources/}"; rel="${rel#/Library/Frameworks/R.framework/Resources/}"
      # quiet: it warns on every file that the signature is now invalid (re-signed below); a failure still stops the script
      install_name_tool -change "$old" "@executable_path/../../$rel" "$f" 2>/dev/null; changed=1
    done
    id=$(otool -D "$f" | tail -n +2 | head -1)
    if [[ "$id" == /Library/Frameworks/R.framework/* ]]; then
      rel="${id#/Library/Frameworks/R.framework/Versions/*/Resources/}"; install_name_tool -id "@executable_path/../../$rel" "$f" 2>/dev/null; changed=1
    fi
    if [ $changed = 1 ]; then codesign --force -s - "$f" 2>/dev/null; n=$((n + 1)); fi
  done < <(find "$1" -type f \( -name '*.so' -o -name '*.dylib' -o -perm -u+x \) -print0)
  echo "  $n files rewritten"
}

echo "1/6 Copying R $RVER ($ARCH) from $SRC"
rm -rf "$OUT"; mkdir -p "$R/library"
rsync -a --exclude 'library/' --exclude 'doc/' --exclude 'tests/' --exclude '*.dSYM' "$SRC/" "$R/"
for p in $(ls "$SRC/library"); do
  pri=$(grep -m1 '^Priority:' "$SRC/library/$p/DESCRIPTION" 2>/dev/null | awk '{print $2}' || true)
  if [ "$pri" = base ] || [ "$pri" = recommended ]; then cp -R "$SRC/library/$p" "$R/library/"; fi
done

echo "2/6 bin/R finds its home from where it is; R's own files relocated"
sed -i '' 's|^R_HOME_DIR=.*|R_HOME_DIR="$(cd "$(dirname "$0")/.." \&\& pwd -P)"|' "$R/bin/R"
# share (time zones), include and doc from there too: left absolute, R would read the installed R's, or no time zones at all
sed -i '' 's|^\(R_[A-Z]*_DIR\)="/Library/Frameworks/R.framework/Resources/\([a-z]*\)"|\1="${R_HOME_DIR}/\2"|' "$R/bin/R"
! grep -n '/Library/Frameworks/R.framework' "$R/bin/R" || { echo "  bin/R still names /Library/Frameworks/R.framework" >&2; exit 1; }
sed -i '' "s|/Library/Frameworks/R.framework/Versions/[0-9.]*/Resources|\${R_HOME}|g; s|/Library/Frameworks/R.framework/Resources|\${R_HOME}|g" "$R/etc/Renviron"
relocate "$R"
RS=(env RHOME="$R" R_LIBS_USER="$OUT/none" R_LIBS_SITE="$OUT/none")
[ ${#RUN[@]} -gt 0 ] && RS=("${RUN[@]}" "${RS[@]}")
RS+=("$R/bin/Rscript")

echo "3/6 The packages build/renv.lock pins"
if [ $COPY_PKGS = 1 ]; then
  RLIB="$R/library" "$FW/bin/Rscript" -e '
  lock <- jsonlite::read_json("build/renv.lock")
  for (p in setdiff(names(lock$Packages), dir(Sys.getenv("RLIB")))) { d <- system.file(package = p)
    if (nzchar(d)) file.copy(d, Sys.getenv("RLIB"), recursive = TRUE) }'
else
  "${RS[@]}" build/runtime_packages.R install "$OUT"
fi

echo "4/6 Relocating the packages, and checking their versions"
relocate "$R/library"
left=$(find "$R" -type f \( -name '*.so' -o -name '*.dylib' \) -exec otool -L {} \; 2>/dev/null | grep -c '/Library/Frameworks/R.framework' || true)
[ "$left" = 0 ] || { echo "  $left references to /Library/Frameworks/R.framework remain" >&2; exit 1; }
"${RS[@]}" build/runtime_packages.R check "$OUT" $([ $NEWER = 1 ] && echo --allow-newer)

echo "5/6 Checking it runs on its own ($ARCH, no user or site library)"
"${RS[@]}" build/runtime_packages.R smoke "$OUT"

echo "6/6 Packing"
( cd "$ROOT/build/out" && tar -czf "$NAME.tar.gz" "$NAME" )
du -sh "$OUT" "$ROOT/build/out/$NAME.tar.gz" | sed 's/^/  /'
if [ $INSTALL = 1 ]; then rm -rf "$ROOT/runtime"; cp -R "$OUT" "$ROOT/runtime"; echo "  installed as runtime/"; fi
