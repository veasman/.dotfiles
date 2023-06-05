#/bin/bash

DEPS="stow gcc curl xwallpaper zsh ripgrep sxhkd dunst libnotify-bin xcompmgr i3 tmux npm fzf light blueman playerctl"

change_shell() {
    PASSWORD=$(whiptail --passwordbox "Please enter your password to change the default shell to zsh:" 8 78 --title "Change Shell" 3>&1 1>&2 2>&3)
    exitstatus=$?
    if [ $exitstatus = 0 ]; then
        echo $PASSWORD | chsh -s /bin/zsh > /dev/null 2>&1
    else
        echo "User chose not to change the default shell."
    fi
}

install_apt_packages() {
    sudo apt update
    sudo apt upgrade

    # Nvidia
    sudo ubuntu-drivers autoinstall

    sudo apt install $DEPS -y
}

install_neovim() {
    whiptail --title "Neovim Installer" --yesno "Would you like to install Neovim?" 10 60

    exit_status=$?
    if [ $exit_status -eq 0 ]; then
        sudo add-apt-repository ppa:neovim-ppa/unstable
        sudo apt update
        sudo apt install neovim

        # Install packer
        git clone --depth 1 https://github.com/wbthomason/packer.nvim\
         ~/.local/share/nvim/site/pack/packer/start/packer.nvim

        whiptail --title "Neovim Installer" --msgbox "Neovim installation completed successfully!" 10 60
    else
        whiptail --title "Neovim Installer" --msgbox "Neovim installation cancelled." 10 60
    fi
}

install_doom_emacs() {
    whiptail --title "Doom Emacs Installer" --yesno "Would you like to install Doom Emacs?" 10 60

    exit_status=$?
    if [ $exit_status -eq 0 ]; then
        # I think I'm missing emacs 29 install here...
        sudo apt install fd-find libvterm-bin
        git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
        ~/.emacs.d/bin/doom install

        whiptail --title "Doom Emacs Installer" --msgbox "Doom Emacs installation completed successfully!" 10 60
    else
        whiptail --title "Doom Emacs Installer" --msgbox "Doom Emacs installation cancelled." 10 60
    fi
}

display_infobox() {
    output_file=$(mktemp)

    # Execute the function and redirect its output to the temporary file
    $3 >"${output_file}" 2>&1 &

    # Show the tailboxbg while the function is running
    whiptail --title "$1" --tailboxbg "$output_file" 10 60

    # Cleanup
    rm "${output_file}"
}

install_fonts() {
    mkdir -p ~/.local/share/fonts/

    # There is no better font
    wget https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip
    mkdir fira
    mv Fira_Code_v6.2.zip fira/
    cd fira/
    unzip Fira_Code_v6.2.zip
    mv ttf/* ~/.local/share/fonts/
    rm -rf Fira_Code_v6.2.zip

    cd ..
    rm -rf fira

    fc-cache -v
}

install_zsh_plugins() {
    sudo mkdir -p /usr/share/zsh/plugins
    git clone git@github.com:zsh-users/zsh-autosuggestions.git
    git clone git@github.com:zdharma-continuum/fast-syntax-highlighting.git

    sudo mv zsh-autosuggestions /usr/share/zsh/plugins
    sudo mv fast-syntax-highlighting /usr/share/zsh/plugins
}

install_docker() {
    # Install deps
    sudo apt install ca-certificates curl gnupg

    # Add Docker’s official GPG key
    sudo install -m 0755 -d /etc/apt/keyrings

    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

    sudo chmod a+r /etc/apt/keyrings/docker.gpg

    # Set up the repository
    echo \
        "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
        "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
        sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

    sudo apt update

    # Install docker
    sudo apt install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

    sudo usermod -aG docker $USER
}

install_apt_packages
change_shell
install_fonts
#install_doom_emacs
install_rustup
install_neovim
install_zsh_plugins
install_docker

# Setup is done, let's link the config files
#rm ~/.profile
#stow bin i3 kitty nvim shell startup sxhkd wallpapers

echo "Please add the following line to your sudoers file:"
echo "$USER ALL=(ALL) NOPASSWD: /usr/bin/light"
