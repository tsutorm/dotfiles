# グローバルな npm パッケージと agent-browser の Chrome。
manager=$(cat "$HOME/.config/dotfiles/runtime-manager" 2>/dev/null || echo asdf)
if [ "$manager" = mise ]; then
    export PATH="$HOME/.local/bin:$PATH"; has mise && eval "$(mise activate bash --shims)"
else
    export PATH="$HOME/bin:$HOME/.asdf/shims:$PATH"
fi
if [ "$DRY" = 1 ] && ! has npm; then
    log "[dry-run] npm install -g $(grep -vE '^\s*(#|$)' "$DOTFILES/packages/npm.txt" | tr '\n' ' ')"
    log "[dry-run] agent-browser install --with-deps"
    return 0
fi
has npm || { echo "npm が見つかりません（runtime ステップを先に実行）" >&2; exit 1; }

missing=()
for p in $(grep -vE '^\s*(#|$)' "$DOTFILES/packages/npm.txt"); do
    npm ls -g --depth=0 "$p" >/dev/null 2>&1 || missing+=("$p")
done
if [ ${#missing[@]} -gt 0 ]; then run npm install -g "${missing[@]}"; else log "ok    npm globals"; fi
# npm で入れたコマンドの shim を作る（asdf も mise も npm install -g では自動で作らない）
case $manager in
    mise) run mise reshim ;;
    *)    run asdf reshim nodejs ;;
esac

# Chrome for Testing と、その実行に要るシステムライブラリ
if has agent-browser; then
    if [ "$(id -u)" = 0 ] || sudo -n true 2>/dev/null; then run agent-browser install --with-deps
    else run agent-browser install; fi
fi
