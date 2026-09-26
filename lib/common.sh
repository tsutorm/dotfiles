# bootstrap.sh と steps/*.sh が共有する関数。DRY=1 なら変更せず表示だけする。
DOTFILES="${DOTFILES:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
DRY="${DRY:-0}"
BACKUP_DIR="${BACKUP_DIR:-$HOME/.dotfiles-backup/$(date +%Y%m%d%H%M%S)}"

log()  { printf '%s\n' "$*"; }
step() { printf '\n== %s\n' "$*"; }
has()  { command -v "$1" >/dev/null 2>&1; }
run()  { if [ "$DRY" = 1 ]; then log "[dry-run] $*"; else "$@"; fi; }

# root 権限が要る処理。root ならそのまま、そうでなければ sudo を付ける。
as_root() { if [ "$(id -u)" = 0 ]; then run "$@"; else run sudo "$@"; fi; }

# src を dst へのシンボリックリンクにする。既存の実ファイルは BACKUP_DIR に退避する。
link() {
    local src=$1 dst=$2
    if [ -L "$dst" ] && [ "$(readlink "$dst")" = "$src" ]; then log "ok    $dst"; return; fi
    run mkdir -p "$(dirname "$dst")"
    if [ -e "$dst" ] || [ -L "$dst" ]; then
        local rel=${dst#"$HOME"/}
        run mkdir -p "$BACKUP_DIR/$(dirname "$rel")"
        run mv "$dst" "$BACKUP_DIR/$rel"
    fi
    run ln -s "$src" "$dst"
    log "link  $dst -> $src"
}

# file に line が無ければ末尾に足す。
ensure_line() {
    local file=$1 line=$2
    if [ -f "$file" ] && grep -qxF "$line" "$file"; then log "ok    $file"; return; fi
    if [ "$DRY" = 1 ]; then log "[dry-run] append to $file: $line"; return; fi
    mkdir -p "$(dirname "$file")"
    printf '\n%s\n' "$line" >>"$file"
    log "add   $file"
}

is_wsl() { grep -qi microsoft /proc/version 2>/dev/null; }
