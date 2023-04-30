#!/bin/bash

wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/FiraCode.zip
mkdir -p ~/.local/share/fonts/
unzip FiraCode ~/.local/share/fonts/
fc-cache -v
