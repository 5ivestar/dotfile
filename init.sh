#!/bin/bash

#checking prereq commands
prereq="zsh emacs"
for c in ${prereq}
do
    if !(type $c > /dev/null 2>&1); then
        echo "error!!! command $c is not installed"
        exit 1
    fi
done

ln -sf ~/dotfile/.vimrc ~/.vimrc
if [ -e ~/.emacs.d ];then
    mv ~/.emacs.d ~/.emacs.d2
fi
ln -sf ~/dotfile/.emacs.d ~/.emacs.d

git config --global core.editor emacs

if [ ! -d ~/.oh-my-zsh ]
then
    echo "Installing oh-my-zsh"
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
    chsh -s /bin/bash

    # download auto suggestion
    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions

    ### to add new plugins, add download plugin download command here and modify .zshrc to add the plugin ##
    #plugins=(
    #   git
    #   zsh-autosuggestions
    #)
    
    if [ -e ~/.zshrc ]
    then
        echo "Backuping existing .zshrc file into .zshrc.bak"
        cp ~/.zshrc ./.zshrc.bak
    fi
    cp ~/dotfile/.zshrc ~/.zshrc
fi

#at last source .bashrc
source ~/.zshrc
