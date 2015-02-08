# dotfiles


## emacs & cask setup
wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-24.3.tar.gz
tar zxf emacs-24.3.tar.gz
cd emacs-24.3
./configure --without-x --without-selinux --without-sound
make
sudo make install
echo alias emacs='/usr/local/bin/emacs-24.3' >> ~/.bash_profile
source ~/.bash_profile


curl -fsSkL https://raw.github.com/cask/cask/master/go | python
echo >> export PATH="$HOME/.cask/bin:$PATH" >> ~/.bash_profile
source ~/.bash_profile

cd ~/.emacs.d/
cask install

## auto-install initialize
mkdir auto-install

