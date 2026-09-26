# APM（https://github.com/microsoft/apm）。インストーラが .bashrc / .profile に PATH 設定を追記する。
export PATH="$HOME/.local/bin:$PATH"
has apm && { log "ok    apm"; return 0; }
run bash -c 'curl -sSL https://raw.githubusercontent.com/microsoft/apm/main/install.sh | sh'
