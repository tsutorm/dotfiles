#!/usr/bin/env bash
# Claude Code のステータスライン + 使用量モニタを ~/.claude に導入する。何度実行しても同じ結果になる。
#   ./claude/install.sh           … 導入
#   ./claude/install.sh --dry-run … 何が変わるかだけ表示
set -euo pipefail
SRC="$(cd "$(dirname "$0")" && pwd)"
DEST="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
DRY=0; [ "${1:-}" = "--dry-run" ] && DRY=1
run() { if [ $DRY = 1 ]; then echo "[dry-run] $*"; else "$@"; fi; }

for cmd in jq git; do command -v "$cmd" >/dev/null || { echo "要インストール: $cmd" >&2; exit 1; }; done
mkdir -p "$DEST/bin" "$DEST/backups" "$DEST/state/usage"
ts=$(date +%Y%m%d%H%M%S)

# 1. スクリプトはリポジトリへのシンボリックリンクにする（リポジトリを更新すれば反映される）
link() {
  local from=$1 to=$2
  if [ -L "$to" ] && [ "$(readlink "$to")" = "$from" ]; then echo "ok    $to"; return; fi
  [ -e "$to" ] && run mv "$to" "$DEST/backups/$(basename "$to").$ts"
  run ln -s "$from" "$to"; echo "link  $to -> $from"
}
link "$SRC/statusline.sh"      "$DEST/statusline.sh"
link "$SRC/bin/claude-usage"   "$DEST/bin/claude-usage"

# 2. settings.json: statusLine は置き換え、hooks は同じ command が無いときだけ追記
settings="$DEST/settings.json"; [ -s "$settings" ] || echo '{}' > "$settings"
merged=$(jq --slurpfile f "$SRC/settings.fragment.json" '
  ($f[0]) as $frag
  | .statusLine = $frag.statusLine
  | reduce ($frag.hooks | to_entries[]) as $e (.;
      ([.hooks[$e.key][]?.hooks[]?.command]) as $have
      | .hooks[$e.key] = ((.hooks[$e.key] // [])
          + [$e.value[] | select(any(.hooks[]; .command as $c | $have | index($c)) | not)]))
' "$settings")
if [ "$merged" = "$(jq . "$settings")" ]; then echo "ok    $settings"
else
  run cp "$settings" "$DEST/backups/settings.json.$ts"
  if [ $DRY = 1 ]; then diff <(jq . "$settings") <(echo "$merged") || true
  else echo "$merged" > "$settings"; fi
  echo "merge $settings"
fi

# 3. CLAUDE.md: CLAUDE.*.md ごとに、先頭行のマーカー間を差し替え、無ければ末尾に追記
md="$DEST/CLAUDE.md"; touch "$md"
for frag in "$SRC"/CLAUDE.*.md; do
  begin=$(head -n1 "$frag")                       # 例: <!-- usage-monitor:begin -->
  end=${begin/:begin/:end}
  if grep -qF "$begin" "$md"; then
    new=$(awk -v f="$frag" -v b="$begin" -v e="$end" '
      $0 == b { while ((getline l < f) > 0) print l; skip=1; next }
      $0 == e { skip=0; next }
      !skip' "$md")
  else
    new="$(cat "$md")"$'\n\n'"$(cat "$frag")"
  fi
  if [ "$new" = "$(cat "$md")" ]; then echo "ok    $md ($(basename "$frag"))"
  else
    [ -e "$DEST/backups/CLAUDE.md.$ts" ] || run cp "$md" "$DEST/backups/CLAUDE.md.$ts"   # 変更前の状態を 1 回だけ退避
    [ $DRY = 1 ] || printf '%s\n' "$new" > "$md"
    echo "update $md ($(basename "$frag"))"
  fi
done
echo "完了。次回のステータスライン更新（最長30秒）から反映されます。"
