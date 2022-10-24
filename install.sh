#!/bin/sh

# Essentials
sudo apt install git curl wget sxhkd dunst flameshot fonts-cantarell stow

# Setting up dotfiles

# Emacs
sudo apt remove --autoremove emacs
[ $(grep ^ /etc/apt/sources.list /etc/apt/sources.list.d/* | grep -c "kelleyk") -eq 0 ] && sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs28-nativecomp

# EXWM
sudo ln -sf ~/.emacs.d/exwm/exwm.desktop /usr/share/xsessions/exwm.desktop

# Librewolf
distro=$(if echo " bullseye focal impish jammy uma una " | grep -q " $(lsb_release -sc) "; then echo $(lsb_release -sc); else echo focal; fi)
echo "deb [arch=amd64] http://deb.librewolf.net $distro main" | sudo tee /etc/apt/sources.list.d/librewolf.list
sudo wget https://deb.librewolf.net/keyring.gpg -O /etc/apt/trusted.gpg.d/librewolf.gpg
sudo apt update
sudo apt install librewolf

# Fira Code NF (This is a mess...)
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip
sudo mkdir /tmp/fira/
sudo mv FiraCode.zip /tmp/fira/
cd /tmp/fira || exit 1
unzip FiraCode.zip
rm FiraCode.zip
mkdir ~/.local/share/fonts/
sudo mv ./* ~/.local/share/fonts/
sudo fc-cache -fv

# SSH Key
if [ ! -f ~/.ssh/id_rsa ]; then
    ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa -N ""
    echo "========================="
    echo "         SSH KEY         "
    echo "========================="
    cat ~/.ssh/id_rsa.pub
fi
