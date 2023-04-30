#/bin/sh

DEPS=$(cat install.list | tr '\n' ' ')

sudo apt update

sudo apt install $DEPS -y

echo $@ >> install.txt

sudo snap install alacritty --classic

./fira-code-install.sh
./nvim-install.sh
./nvm-install.sh
