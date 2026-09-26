# ランタイム管理ツール（asdf / mise）を入れ、~/.tool-versions の全ツールをインストールする。
# 選択は RUNTIME_MANAGER、無ければ前回の記録、それも無ければ asdf。
ASDF_VERSION=0.18.0
state="$HOME/.config/dotfiles/runtime-manager"
manager=${RUNTIME_MANAGER:-$(cat "$state" 2>/dev/null || echo asdf)}
case $manager in asdf|mise) ;; *) echo "RUNTIME_MANAGER は asdf か mise: $manager" >&2; exit 2 ;; esac
run mkdir -p "$(dirname "$state")"
[ "$DRY" = 1 ] || echo "$manager" >"$state"
log "runtime manager: $manager"

case $manager in
asdf)
    export ASDF_DATA_DIR="$HOME/.asdf"
    export PATH="$HOME/bin:$ASDF_DATA_DIR/shims:$PATH"
    if [ "$(asdf --version 2>/dev/null | awk '{print $3}')" != "v$ASDF_VERSION" ]; then
        run mkdir -p "$HOME/bin"
        run bash -c "curl -fsSL https://github.com/asdf-vm/asdf/releases/download/v$ASDF_VERSION/asdf-v$ASDF_VERSION-linux-amd64.tar.gz | tar -xz -C '$HOME/bin' asdf"
    fi
    plugins=$(asdf plugin list 2>/dev/null || true)
    for tool in $(awk '!/^#/ && NF {print $1}' "$DOTFILES/home/.tool-versions"); do
        grep -qx "$tool" <<<"$plugins" || run asdf plugin add "$tool"
    done
    (cd "$HOME" && run asdf install)
    ;;
mise)
    export PATH="$HOME/.local/bin:$PATH"
    has mise || run bash -c 'curl -fsSL https://mise.run | sh'
    (cd "$HOME" && run mise install --yes)
    [ "$DRY" = 1 ] || eval "$(mise activate bash --shims)"
    ;;
esac
