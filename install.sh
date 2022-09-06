cd ~

# Docker
sudo apt-get -y remove docker docker-engine docker.io containerd runc
sudo apt-get update
sudo apt-get -y install \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable edge test"
sudo apt-get update
sudo apt-get -y install docker-ce docker-ce-cli containerd.io && usermod -aG docker `whoami`

# emacs-bin
sudo apt-get install -y autoconf build-essential pkg-config libncurses-dev gnutls-dev gnutls-bin libgccjit-10-dev zlib1g-dev texinfo libgccjit0
mkdir -p ~/src/
cd ~/src
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure --with-native-compilation --without-x
make -j$(nproc)
sudo make install
cd ~

# emacs - mozc
sudo apt-get install -y emacs-mozc-bin

# ag(silver-searcher)
sudo apt-get install -y silversearcher-ag

# ASDF and plugins
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1
cat <<'EOF' >> ~/.bashrc
. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash
EOF
source ~/.bashrc
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf plugin-add rust https://github.com/asdf-community/asdf-rust.git

# Deno
curl -fsSL https://deno.land/x/install/install.sh | sh
cat <<'EOF' >> ~/.bashrc
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
EOF
source ~/.bashrc

