#!/usr/bin/env bash
# クリーンインストールした WSL2 Ubuntu 24.04 から開発環境を復帰する。何度実行しても同じ結果になる。
#   ./bootstrap.sh                    全ステップ
#   ./bootstrap.sh --only home,runtime / --skip apt
#   ./bootstrap.sh --with-rocm        AMD GPU / ROCm も入れる
#   ./bootstrap.sh --dry-run          変更内容だけ表示
#   RUNTIME_MANAGER=mise ./bootstrap.sh   ランタイム管理を mise にする（既定は asdf）
set -euo pipefail
export DOTFILES="$(cd "$(dirname "$0")" && pwd)"
STEPS=(apt home runtime globals claude apm wsl)

only=""; skip=""; rocm=0; plan=0
while [ $# -gt 0 ]; do
    case $1 in
        --only) only=$2; shift ;;
        --skip) skip=$2; shift ;;
        --with-rocm) rocm=1 ;;
        --dry-run) export DRY=1 ;;
        --plan) plan=1 ;;
        -h|--help) sed -n '2,8p' "$0"; exit 0 ;;
        *) echo "unknown option: $1" >&2; exit 2 ;;
    esac
    shift
done

all=("${STEPS[@]}")
[[ $rocm = 1 || ",$only," == *",rocm,"* ]] && all=(apt rocm "${STEPS[@]:1}")
for s in ${only//,/ } ${skip//,/ }; do
    [ -f "$DOTFILES/steps/$s.sh" ] || { echo "unknown step: $s" >&2; exit 2; }
done
selected=()
for s in "${all[@]}"; do
    [ -n "$only" ] && [[ ",$only," != *",$s,"* ]] && continue
    [[ ",$skip," == *",$s,"* ]] && continue
    selected+=("$s")
done

if [ $plan = 1 ]; then printf '%s\n' "${selected[@]}"; exit 0; fi

. "$DOTFILES/lib/common.sh"
for s in "${selected[@]}"; do
    step "$s"
    . "$DOTFILES/steps/$s.sh"
done
log ""; log "完了。新しいシェルを開くと設定が反映されます。"
