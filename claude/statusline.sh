#!/usr/bin/env bash
# Claude Code statusline: 2 行表示 + 使用量スナップショットを保存（claude-usage / usage-guard が読む）
input=$(cat)

IFS=$'\t' read -r sid sname model effort style cwd wt ctx cost r5 r5at r7 r7at ctxsize < <(
  jq -r '[
    .session_id // "",
    .session_name // "",
    .model.display_name // .model.id // "?",
    .effort.level // "-",
    .output_style.name // "default",
    .workspace.current_dir // .cwd // "",
    .worktree.name // .workspace.git_worktree // "",
    (.context_window.used_percentage // ""),
    (.cost.total_cost_usd // 0),
    (.rate_limits.five_hour.used_percentage // ""),
    (.rate_limits.five_hour.resets_at // ""),
    (.rate_limits.seven_day.used_percentage // ""),
    (.rate_limits.seven_day.resets_at // ""),
    (.context_window.context_window_size // "")
  ] | map(tostring | if . == "" then "_" else . end) | @tsv' <<<"$input"
)
for v in sid sname cwd wt ctx r5 r5at r7 r7at ctxsize; do [ "${!v}" = "_" ] && printf -v "$v" ''; done

branch=$(git -C "$cwd" branch --show-current 2>/dev/null)
[ -z "$branch" ] && branch=$(git -C "$cwd" rev-parse --short HEAD 2>/dev/null)

# スナップショット保存（アトミック）
if [ -n "$sid" ]; then
  dir="$HOME/.claude/state/usage"; mkdir -p "$dir"
  jq -c --argjson now "$(date +%s)" --arg branch "$branch" '{
      updated_at: $now, session_id, session_name, model: .model, effort: .effort.level,
      output_style: .output_style.name, cwd: (.workspace.current_dir // .cwd), branch: $branch,
      worktree: (.worktree.name // .workspace.git_worktree),
      context: .context_window, cost_usd: .cost.total_cost_usd, rate_limits
    }' <<<"$input" >"$dir/$sid.json.tmp" 2>/dev/null && mv -f "$dir/$sid.json.tmp" "$dir/$sid.json"
fi

# 控えめな 256 色パレット（彩度・明度を落とした色）
R=$'\e[0m'; DIM=$'\e[38;5;243m'; B=''
C_CYAN=$'\e[38;5;109m'; C_MAG=$'\e[38;5;139m'; C_BLUE=$'\e[38;5;67m'; C_YEL=$'\e[38;5;144m'
C_OK=$'\e[38;5;65m'; C_WARN=$'\e[38;5;137m'; C_CRIT=$'\e[38;5;131m'; C_EMPTY=$'\e[38;5;238m'

color_for() { # pct warn crit
  local p=${1%.*}; p=${p:-0}
  if [ "$p" -ge "$3" ]; then printf '%s' "$C_CRIT"; elif [ "$p" -ge "$2" ]; then printf '%s' "$C_WARN"; else printf '%s' "$C_OK"; fi
}
bar() { # label pct warn crit [suffix]
  local label=$1 pct=$2
  if [ -z "$pct" ]; then printf '%s%s ──────────  n/a%s' "$DIM" "$label" "$R"; return; fi
  local p=${pct%.*}; p=${p:-0}; [ "$p" -gt 100 ] && p=100
  local fill=$(( (p + 5) / 10 )) s="" e="" i
  for ((i=0;i<10;i++)); do [ $i -lt $fill ] && s+="█" || e+="█"; done
  printf '%s%s%s %s%s%s%s%s %3d%%%s' "$DIM" "$label" "$R" "$(color_for "$pct" "$3" "$4")" "$s" "$C_EMPTY" "$e" "$R" "$p" "${5:+ $DIM$5$R}"
}
reset_in() { # epoch -> "2h13m" / "3d4h"
  [ -z "$1" ] && return
  local d=$(( $1 - $(date +%s) )); [ $d -lt 0 ] && d=0
  if [ $d -ge 86400 ]; then printf '↻%dd%dh' $((d/86400)) $((d%86400/3600))
  else printf '↻%dh%02dm' $((d/3600)) $((d%3600/60)); fi
}

short_dir=${cwd/#$HOME/\~}
sid_short=${sid:0:8}
ident="${C_MAG}⌘ ${sname:+$sname }${DIM}[$sid_short]${R}"

line1="${B}${C_CYAN}${model}${R} ${DIM}effort:${R}${effort} ${DIM}style:${R}${style}  ${C_BLUE}${short_dir}${R}"
[ -n "$branch" ] && line1+="  ${C_YEL}⎇ ${branch}${R}"
[ -n "$wt" ] && line1+="  ${DIM}wt:${R}${wt}"
line1+="  ${ident}"

ctx_sfx=""; [ -n "$ctxsize" ] && ctx_sfx="/$((ctxsize/1000))k"
line2="$(bar ctx "$ctx" 60 80 "$ctx_sfx")  $(bar 5h "$r5" 70 90 "$(reset_in "$r5at")")  $(bar 7d "$r7" 80 95 "$(reset_in "$r7at")")  ${B}\$$(printf '%.2f' "$cost")${R}"

printf '%s\n%s\n' "$line1" "$line2"
