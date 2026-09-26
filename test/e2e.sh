#!/usr/bin/env bash
# E2E: まっさらな ubuntu:24.04 コンテナで bootstrap を最初から最後まで通す（Docker とネットワークが必要。15〜30 分）。
#   ./test/e2e.sh            asdf で全ステップ → 2 回目の実行 → mise への切り替え
#   KEEP=1 ./test/e2e.sh     終了後もコンテナを残す（調査用）
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cname="dotfiles-e2e-$$"
cleanup() { [ "${KEEP:-0}" = 1 ] && echo "container: $cname" || docker rm -f "$cname" >/dev/null 2>&1 || true; }
trap cleanup EXIT

# WSL の既定ユーザー相当：sudo できる一般ユーザー。git はリポジトリを clone する時点で必要なので前提に含める。
docker run -d --name "$cname" ubuntu:24.04 sleep infinity >/dev/null
docker exec "$cname" bash -c 'apt-get update -qq && DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo git >/dev/null \
  && useradd -m -s /bin/bash dev && echo "dev ALL=(ALL) NOPASSWD:ALL" >/etc/sudoers.d/dev'
# .git を除いた作業ツリーを渡す（worktree の .git はホストのパスを指し、コンテナ内では壊れるため）
docker exec "$cname" mkdir -p /home/dev/dotfiles
tar -C "$ROOT" --exclude=.git -cf - . | docker exec -i "$cname" tar -C /home/dev/dotfiles -xf -
docker exec "$cname" chown -R dev:dev /home/dev/dotfiles
as_dev() { docker exec -u dev -w /home/dev/dotfiles -e USER=dev "$cname" bash -c "$1"; }
login() { as_dev "bash -ic '$1' 2>/dev/null"; }   # 新しい対話シェル（.bashrc を読む）

pass=0; fail=0
check() { local label=$1; shift
  if "$@" >/dev/null 2>&1; then pass=$((pass+1)); echo "ok   $label"; else fail=$((fail+1)); echo "FAIL $label"; fi; }
want() { [ "$(login "$1" | tail -1)" = "$2" ]; }   # コマンドの最終行が期待値と一致

echo "== 1 回目（asdf）"
as_dev './bootstrap.sh' >"${E2E_LOG:-/tmp/e2e-first.log}" 2>&1 || { tail -40 "${E2E_LOG:-/tmp/e2e-first.log}"; exit 1; }
tv=$(cat "$ROOT/home/.tool-versions")
v() { awk -v t="$1" '$1==t {print $2}' <<<"$tv"; }
check "node は .tool-versions の版"      want 'node -v' "v$(v nodejs)"
check "python は .tool-versions の版"    want 'python --version' "Python $(v python)"
check "ruby は .tool-versions の版"      want 'ruby -e "print RUBY_VERSION"' "$(v ruby)"
check "deno は .tool-versions の版"      bash -c "$(declare -f login as_dev); cname=$cname; login 'deno --version' | grep -q 'deno $(v deno)'"
check "uv は .tool-versions の版"        bash -c "$(declare -f login as_dev); cname=$cname; login 'uv --version' | grep -q 'uv $(v uv)'"
for c in gh jq direnv tmux docker gcloud agent-browser claude apm; do
  check "$c が使える"                    login "command -v $c"
done
check "git の設定（alias）が効く"        want 'git config alias.st' 'status -s'
check "関数 codex-landlord が定義される" login 'type codex-landlord'
check "Claude のステータスラインが入る"  as_dev 'test -L ~/.claude/statusline.sh'
check "docker グループに入る"            as_dev 'id -nG dev | grep -qw docker'

echo "== 2 回目（冪等性）"
out=$(as_dev './bootstrap.sh' 2>&1) && rc=0 || rc=$?
check "2 回目も成功する"                 test "$rc" = 0
check "2 回目はリンクも追記も行わない"   bash -c '! grep -qE "^(link|add|update) "' <<<"$out"

echo "== mise への切り替え"
as_dev 'RUNTIME_MANAGER=mise ./bootstrap.sh --only runtime,globals' >/tmp/e2e-mise.log 2>&1 && rc=0 || rc=$?
check "mise で runtime が成功する"       test "$rc" = 0
check "node が mise 管理になる"          bash -c "$(declare -f login as_dev); cname=$cname; login 'command -v node' | grep -q mise"
check "mise でも node は同じ版"          want 'node -v' "v$(v nodejs)"
check "mise でも agent-browser が使える"  login "command -v agent-browser"
check "asdf の shims が PATH から外れる" bash -c "! ( $(declare -f login as_dev); cname=$cname; login 'echo \$PATH' | grep -q .asdf/shims )"

echo "---- $pass passed, $fail failed"
[ "$fail" -eq 0 ]
