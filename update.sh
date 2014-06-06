#!/usr/bin/env
declare -A FILES
FILES=( [.emacs]=emacs
        [.vimrc]=vim
        [.bash_profile]=shell
        [.bashrc]=shell
        [.inputrc]=shell
        [.config/awesome/keydoc.lua]=awesome
        [.config/awesome/rc.lua]=awesome
        [.config/awesome/runonce.lua]=awesome)

for file in ${!FILES[*]}; do
  cp $HOME/$file ${FILES[$file]}
done
