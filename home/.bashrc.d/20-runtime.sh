# ランタイム管理ツール（asdf / mise）の初期化。どちらを使うかは bootstrap の runtime ステップが
# ~/.config/dotfiles/runtime-manager に記録する。バージョンは ~/.tool-versions が正。
case "$(cat "$HOME/.config/dotfiles/runtime-manager" 2>/dev/null || echo asdf)" in
    mise)
        command -v mise >/dev/null && eval "$(mise activate bash)" ;;
    *)
        export ASDF_DATA_DIR="$HOME/.asdf"
        export PATH="$ASDF_DATA_DIR/shims:$PATH" ;;
esac
