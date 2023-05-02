#!/bin/bash

sudo mkdir -p /usr/share/zsh/plugins
git clone git@github.com:zsh-users/zsh-autosuggestions.git
git clone git@github.com:zdharma-continuum/fast-syntax-highlighting.git

sudo mv zsh-autosuggestions /usr/share/zsh/plugins
sudo mv fast-syntax-highlighting /usr/share/zsh/plugins
