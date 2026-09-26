#!/usr/bin/env bash
# claude/ の振る舞いテスト。一時 HOME の中だけで実行し、実環境の ~/.claude には触れない。
#   ./claude/test.sh
set -uo pipefail
SRC="$(cd "$(dirname "$0")" && pwd)"
unset "${!CLAUDE_USAGE_@}"   # 閾値は既定値で検証する
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
export HOME="$T/home"; mkdir -p "$HOME/.claude"
pass=0; fail=0
check() { # name, command...
  local name=$1; shift
  if "$@" >/dev/null 2>&1; then pass=$((pass+1)); echo "ok   $name"
  else fail=$((fail+1)); echo "FAIL $name"; fi
}
strip() { sed 's/\x1b\[[0-9;]*m//g'; }
now=$(date +%s)
input() { # sid ctx 5h 7d
  cat <<J
{"session_id":"$1","session_name":"test","model":{"display_name":"Opus"},"effort":{"level":"high"},
 "output_style":{"name":"default"},"workspace":{"current_dir":"$SRC"},"cost":{"total_cost_usd":1.234},
 "context_window":{"context_window_size":200000,"used_percentage":$2},
 "rate_limits":{"five_hour":{"used_percentage":$3,"resets_at":$((now+3600))},"seven_day":{"used_percentage":$4,"resets_at":$((now+90000))}}}
J
}
SL="$SRC/statusline.sh"; CU="$SRC/bin/claude-usage"
hook() { echo "{\"session_id\":\"$1\",\"hook_event_name\":\"$2\"}" | "$CU" --hook; }

# --- statusline.sh
out=$(input s1 65 92 41 | "$SL" | strip)
check "statusline は 2 行を出す"                     test "$(wc -l <<<"$out")" -eq 2
check "1 行目にモデル・effort・短縮 session_id"      grep -q 'Opus effort:high style:default .*⌘ test \[s1\]' <<<"$out"
check "2 行目に ctx/5h/7d の % と cost"              grep -qE 'ctx .* 65% .*5h .* 92% .*7d .* 41% .*\$1\.23' <<<"$out"
check "使用量スナップショットを保存する"             test -s "$HOME/.claude/state/usage/s1.json"
out=$(echo '{"session_id":"s0","model":{"display_name":"Opus"}}' | "$SL" | strip)
check "rate_limits が無ければ n/a を表示"            grep -q '5h ──────────  n/a' <<<"$out"

# --- claude-usage
out=$("$CU" s1)
check "要約に level と残りトークンを出す"            grep -q 'level=crit ctx=65% (残り約70k tok)' <<<"$out"
check "5h crit の推奨を出す"                         grep -q '5h 枠 逼迫' <<<"$out"
check "スナップショットが無ければ終了コード 1"       bash -c "! '$CU' nosuch"
check "--json は level を持つ JSON"                  bash -c "'$CU' --json s1 | jq -e '.level == 2'"

# --- claude-usage --hook
input s2 10 10 10 | "$SL" >/dev/null
check "全て ok なら何も出さない"                     test -z "$(hook s2 PostToolUse)"
input s2 65 10 10 | "$SL" >/dev/null
check "段階が上がったら additionalContext を出す"    bash -c "$(declare -f hook); CU='$CU'; hook s2 PostToolUse | jq -e '.hookSpecificOutput.additionalContext | test(\"level=warn\")'"
check "同じ段階では PostToolUse で繰り返さない"      test -z "$(hook s2 PostToolUse)"
check "warn 以上なら UserPromptSubmit では毎回出す"  bash -c "$(declare -f hook); CU='$CU'; hook s2 UserPromptSubmit | jq -e '.hookSpecificOutput.hookEventName == \"UserPromptSubmit\"'"
input s2 10 10 10 | "$SL" >/dev/null; hook s2 PostToolUse >/dev/null
input s2 65 10 10 | "$SL" >/dev/null
check "下がってから再上昇したら再通知する"           test -n "$(hook s2 PostToolUse)"

# --- install.sh
C="$HOME/.claude"
echo '{"model":"opus","hooks":{"PostToolUse":[{"matcher":"Edit","hooks":[{"type":"command","command":"echo x"}]}]}}' >"$C/settings.json"
echo '# 既存' >"$C/CLAUDE.md"
"$SRC/install.sh" >/dev/null
check "スクリプトをリポジトリへのリンクにする"       test "$(readlink "$C/statusline.sh")" = "$SL"
check "statusLine を設定する"                        jq -e '.statusLine.command == "~/.claude/statusline.sh"' "$C/settings.json"
check "既存 hook と既存設定を残す"                   jq -e '.model == "opus" and (.hooks.PostToolUse | length == 2)' "$C/settings.json"
check "CLAUDE.md に節を追記し既存を残す"             bash -c "grep -q '# 既存' '$C/CLAUDE.md' && grep -q 'usage-monitor:begin' '$C/CLAUDE.md'"
check "CLAUDE.md にブラウジングの使い分け節を追記する"  grep -q 'browser-rules:begin' "$C/CLAUDE.md"
check "使い分け節は WebFetch と agent-browser の基準を持つ" bash -c "grep -q 'WebFetch' '$C/CLAUDE.md' && grep -q 'skills get core' '$C/CLAUDE.md'"
check "退避した CLAUDE.md は変更前の内容のまま"      bash -c "[ \"\$(cat '$C'/backups/CLAUDE.md.*)\" = '# 既存' ]"
before=$(md5sum "$C/settings.json" "$C/CLAUDE.md")
out=$("$SRC/install.sh")
check "2 回目は何も変えない"                         test "$before" = "$(md5sum "$C/settings.json" "$C/CLAUDE.md")"
check "2 回目は全て ok と報告"                       test "$(grep -c '^ok' <<<"$out")" -eq 5
check "変更前のファイルを backups に退避する"        bash -c "ls '$C'/backups/settings.json.* '$C'/backups/CLAUDE.md.*"

echo "---- $pass passed, $fail failed"
[ "$fail" -eq 0 ]
