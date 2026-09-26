# apt パッケージと外部リポジトリ（Docker・Google Cloud CLI）。
export DEBIAN_FRONTEND=noninteractive
codename=$(. /etc/os-release && echo "$VERSION_CODENAME")

as_root apt-get update -qq
as_root apt-get install -y -qq $(grep -vE '^\s*(#|$)' "$DOTFILES/packages/apt.txt")

# Docker Engine（https://docs.docker.com/engine/install/ubuntu/）
if [ ! -f /etc/apt/sources.list.d/docker.list ]; then
    as_root install -m 0755 -d /etc/apt/keyrings
    as_root curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
    as_root sh -c "echo 'deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu $codename stable' >/etc/apt/sources.list.d/docker.list"
fi
# Google Cloud CLI（https://cloud.google.com/sdk/docs/install#deb）
if [ ! -f /etc/apt/sources.list.d/google-cloud-sdk.list ]; then
    as_root sh -c "curl -fsSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | gpg --dearmor --yes -o /usr/share/keyrings/cloud.google.gpg"
    as_root sh -c "echo 'deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main' >/etc/apt/sources.list.d/google-cloud-sdk.list"
fi
as_root apt-get update -qq
as_root apt-get install -y -qq docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin google-cloud-cli

# sudo なしで docker を使えるようにする（反映は次回ログインから）
if ! id -nG "${USER:-$(id -un)}" | grep -qw docker; then as_root usermod -aG docker "${USER:-$(id -un)}"; fi
