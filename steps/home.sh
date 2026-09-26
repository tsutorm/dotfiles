# home/ 以下のファイルを ~ の同じ位置へリンクし、.bashrc から .bashrc.d/*.sh を読み込ませる。
# .bashrc 自体は Ubuntu 標準のまま残す（他のインストーラが追記する先でもあるため）。
while IFS= read -r -d '' f; do
    rel=${f#"$DOTFILES/home/"}
    link "$f" "$HOME/$rel"
done < <(find "$DOTFILES/home" -type f -print0 | sort -z)

ensure_line "$HOME/.bashrc" 'for f in "$HOME"/.bashrc.d/*.sh; do [ -r "$f" ] && . "$f"; done; unset f'
