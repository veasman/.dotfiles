#/bin/sh

sudo apt update

sudo apt install $@ -y

echo $@ >> install.txt

./fira-code-install.sh
./nvim-install.sh
