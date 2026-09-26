# Claude Code（公式インストーラ）と claude/ の設定。
export PATH="$HOME/.local/bin:$PATH"
has claude || run bash -c 'curl -fsSL https://claude.ai/install.sh | bash'
run "$DOTFILES/claude/install.sh"
