# WSL: URL を Windows 側の Chrome で開く（gh などが BROWSER / GH_BROWSER を見る）。
chrome="/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"
if [ -x "$chrome" ]; then
    export BROWSER="$chrome"
    export GH_BROWSER="'$chrome'"
fi
unset chrome
# ROCm on WSL: GPU を DXG 経由で検出させる。
export HSA_ENABLE_DXG_DETECTION=1
