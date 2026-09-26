# WSL: systemd を有効にする（docker などのサービスに必要。反映は `wsl --shutdown` 後）。
is_wsl || { log "skip  WSL ではない"; return 0; }
if grep -qE '^\s*systemd\s*=\s*true' /etc/wsl.conf 2>/dev/null; then log "ok    /etc/wsl.conf"; return 0; fi
if grep -q '^\[boot\]' /etc/wsl.conf 2>/dev/null; then
    as_root sed -i '/^\[boot\]/a systemd=true' /etc/wsl.conf
else
    as_root sh -c "printf '[boot]\\nsystemd=true\\n' >>/etc/wsl.conf"
fi
log "update /etc/wsl.conf（Windows 側で wsl --shutdown して再起動すると反映）"
