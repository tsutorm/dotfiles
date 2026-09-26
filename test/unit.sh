#!/usr/bin/env bash
# 単体テスト。一時 HOME の中だけで実行し、実環境には触れない（root 権限・ネットワーク不要）。
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
export HOME="$T/home"; mkdir -p "$HOME"
pass=0; fail=0
check() { local name=$1; shift
  if "$@" >/dev/null 2>&1; then pass=$((pass+1)); echo "ok   $name"; else fail=$((fail+1)); echo "FAIL $name"; fi; }

# --- lib/common.sh
. "$ROOT/lib/common.sh"
mkdir -p "$T/src"; echo a >"$T/src/f"
link "$T/src/f" "$HOME/sub/f" >/dev/null
check "link は親ディレクトリを作ってリンクする"       test "$(readlink "$HOME/sub/f")" = "$T/src/f"
out=$(link "$T/src/f" "$HOME/sub/f")
check "link は既に正しければ ok を返す"               grep -q '^ok' <<<"$out"
echo old >"$HOME/g"; link "$T/src/f" "$HOME/g" >/dev/null
check "link は既存の実ファイルを退避する"             bash -c "cat '$HOME'/.dotfiles-backup/*/g | grep -qx old"
echo x >"$HOME/rc"
ensure_line "$HOME/rc" 'source me' >/dev/null; ensure_line "$HOME/rc" 'source me' >/dev/null
check "ensure_line は 1 回だけ追記する"               test "$(grep -c 'source me' "$HOME/rc")" -eq 1
check "ensure_line は既存の内容を残す"                grep -qx x "$HOME/rc"
DRY=1 ensure_line "$HOME/rc2" 'y' >/dev/null
check "DRY=1 では書き込まない"                       test ! -e "$HOME/rc2"

# --- bootstrap.sh のステップ選択
plan() { "$ROOT/bootstrap.sh" --plan "$@"; }
check "既定のステップ順"                              test "$(plan | tr '\n' ' ')" = "apt home runtime globals claude apm wsl "
check "--with-rocm で rocm を apt の後に足す"         test "$(plan --with-rocm | tr '\n' ' ')" = "apt rocm home runtime globals claude apm wsl "
check "--only で指定したものだけ"                     test "$(plan --only home,runtime | tr '\n' ' ')" = "home runtime "
check "--skip で指定したものを除く"                   test "$(plan --skip apt,wsl | tr '\n' ' ')" = "home runtime globals claude apm "
check "--only rocm は --with-rocm なしでも rocm を選ぶ" test "$(plan --only rocm)" = "rocm"
check "未知のステップ名はエラー"                      bash -c "! '$ROOT/bootstrap.sh' --plan --only nosuch"

# --- dry-run（まっさらな HOME・ツール無しでも最後まで表示でき、何も書き込まない）
fresh="$T/fresh"; mkdir -p "$fresh"
out=$(env -i HOME="$fresh" PATH=/usr/bin:/bin "$ROOT/bootstrap.sh" --dry-run --with-rocm 2>&1); rc=$?
check "dry-run は新規マシンでも成功する"              test "$rc" = 0
check "dry-run は全ステップを表示する"                bash -c "grep -q '== wsl' <<<'$(grep '^==' <<<"$out")'"
check "dry-run は HOME に何も作らない"                test -z "$(ls -A "$fresh")"

# --- home ステップ（リンクと .bashrc への読み込み行）
echo '# skel' >"$HOME/.bashrc"
"$ROOT/bootstrap.sh" --only home >/dev/null
check ".gitconfig をリンクする"                       test "$(readlink "$HOME/.gitconfig")" = "$ROOT/home/.gitconfig"
check ".bashrc.d をファイル単位でリンクする"          test -L "$HOME/.bashrc.d/10-path.sh"
check ".tool-versions をリンクする"                   test -L "$HOME/.tool-versions"
check ".bashrc に .bashrc.d の読み込みを 1 行足す"    test "$(grep -c 'bashrc.d' "$HOME/.bashrc")" -eq 1
check ".bashrc の既存内容を残す"                      grep -qx '# skel' "$HOME/.bashrc"
"$ROOT/bootstrap.sh" --only home >/dev/null
check "2 回目も読み込み行は 1 行のまま"               test "$(grep -c 'bashrc.d' "$HOME/.bashrc")" -eq 1
check "対話シェルで関数が読み込まれる"                bash -c "env -i HOME='$HOME' PATH=/usr/bin:/bin bash -ic 'type codex-landlord' 2>/dev/null"
check "既定（asdf）なら asdf の shims を PATH に入れる" \
      bash -c "env -i HOME='$HOME' PATH=/usr/bin:/bin bash -ic 'echo \$PATH' 2>/dev/null | grep -q '.asdf/shims'"
mkdir -p "$HOME/.config/dotfiles"; echo mise >"$HOME/.config/dotfiles/runtime-manager"
check "runtime-manager=mise なら asdf の shims を PATH に入れない" \
      bash -c "! env -i HOME='$HOME' PATH=/usr/bin:/bin bash -ic 'echo \$PATH' 2>/dev/null | grep -q '.asdf/shims'"

echo "---- $pass passed, $fail failed"
[ "$fail" -eq 0 ]
