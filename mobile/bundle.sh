#!/bin/bash
#the files a phone or a tablet runs ChrysaLisp from, as one tar, which the
#app unpacks to a folder of its own the first time it starts, and again
#whenever this tar is a new one. It is the source the system reads, the
#fonts, the data of the apps, and the VP64 objects the emulator runs.
#
#mobile/bundle.sh [out.tar]	default mobile/build/chrysalisp.tar

set -e
out="${1:-$(dirname "$0")/build/chrysalisp.tar}"
mkdir -p "$(dirname "$out")"
out="$(cd "$(dirname "$out")" && pwd)/$(basename "$out")"
cd "$(dirname "$0")/.."

if [ ! -f obj/vp64/VP64/sys/boot_image ] || [ "$(find obj/vp64 -type f | wc -l)" -lt 100 ]
then
	echo "No VP64 objects, build them with: ./run_tui.sh -n 0 -f -s mobile/make_vp64.lisp"
	exit 1
fi

list=$(mktemp)
trap 'rm -f "$list"' EXIT

#what tells the app this tar from the one it last unpacked
stamp=".mobile_stamp"
{
	git rev-parse HEAD 2>/dev/null || echo nogit
	git status --porcelain 2>/dev/null | shasum | cut -d' ' -f1
	find obj/vp64 -type f -exec stat -f "%m %z %N" {} + 2>/dev/null | shasum | cut -d' ' -f1
} > "$stamp"

#the stamp first, the app reads it before all else
echo "$stamp" > "$list"
#the files of the system git knows of, less what a phone has no use for: the
#host source, the scripts of a desktop, the pictures of the README
git ls-files | grep -v -E '^(src/|mobile/|\.|[^/]*\.(png|zip|bat|ps1|sh|md)$|vscode_|snapshot_host$)' >> "$list"
#and the VP64 objects, the emulator's
find obj/vp64 -type f >> "$list"

COPYFILE_DISABLE=1 tar --format ustar --no-mac-metadata -cf "$out" -T "$list"
rm -f "$stamp"
echo "$out $(du -h "$out" | cut -f1) $(wc -l < "$list" | tr -d ' ') files"
