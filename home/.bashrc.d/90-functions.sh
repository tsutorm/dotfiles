# landlord リポジトリで direnv の環境を読み込んだ上で codex を動かす。
codex-landlord() {
    local repo="$HOME/git/hub/tsutorm/landlord"
    direnv exec "$repo" codex --cd "$repo" --sandbox workspace-write --add-dir "$repo/.git" "$@"
}
