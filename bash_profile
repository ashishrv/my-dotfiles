#------------------------------------------------
# BASH PROFILE - Entry Point
#------------------------------------------------

echo -e "\033[1;33mLoading profiles ... \033[0m"
echo

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

# PRIVATE AND PERSONAL SCRIPTS
test -d "${HOME}/.privatescripts" && export PATH=${HOME}/.privatescripts:${PATH}
test -d "${HOME}/.myscripts" &&  export PATH=${HOME}/.myscripts:${PATH}


# SOURCE - Private profile
test -e "${HOME}/.private_bashprofile" && source "${HOME}/.private_bashprofile"

# SOURCE - OTHER PROFILE FRAGMENTS
test -e "${PROFILE}/default_bashprofile" && source "${PROFILE}/default_bashprofile"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

echo -e "\033[1;33m... which was last updated on\033[0m \033[1;31m$(stat -f "%Sm" $HOME/Dotfiles/.git/refs/heads/master)\033[0m"
echo


