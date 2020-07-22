#------------------------------------------------
# ZSH - Entry Point
# .zprofile is equivalent to .bash_profile and runs at login, including over SSH
# .zshrc is equivalent to .bashrc and runs for each new Terminal session
#------------------------------------------------

#------------------------------------------------
# How to test your shell scripts
# To test script compatibility with Bourne-compatible shells in macOS Catalina, you can change /var/select/sh to /bin/bash, /bin/dash, or /bin/zsh. If you change /var/select/sh to a shell other than bash, be aware that scripts that make use of bashisms may not work properly.
# zsh can be made to emulate sh by executing the command zsh --emulate sh.
#------------------------------------------------

#------------------------------------------------
# Loads everything from zshrc
#------------------------------------------------

export PATH="$HOME/.poetry/bin:$PATH"
