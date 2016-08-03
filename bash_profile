############################
##
## BASH PROFILE
##
############################

if [ -f /etc/profile ]; then
    PATH=""
    source /etc/profile
fi

# DOTFILES Directory
export DOTFILES=$HOME/Dotfiles
# PROFILES DIRECTORY
export PROFILE=$HOME/.profiles
# Keep oldclass paths if possible
export OLDCLASSPATH=$CLASSPATH

test -d "${HOME}/.privatescripts" && export PATH=${HOME}/.privatescripts:${PATH}
test -d "${HOME}/.myscripts" &&  export PATH=${HOME}/.myscripts:${PATH}
# Source private profile, which contains private paths
test -e "${HOME}/.private_bashprofile" && source "${HOME}/.private_bashprofile"
test -e "${PROFILE}/default_bashprofile" && source "${PROFILE}/default_bashprofile"

export PATH="/Users/ashish_vidyarthi/ashish_work/bin:/usr/local/git/bin:$PATH"


# SET PROPMT
symbol="⚡ "
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
export PS1='$symbol\[\e[36;1m\]\W\[\e[0m\] $(__git_ps1 "[\[\e[0;32m\]%s\[\e[0m\]\[\e[0;33m\]$(parse_git_dirty)\[\e[0m\]]")\$ \[\e[0m\]'
