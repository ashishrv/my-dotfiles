# bashrc should source all default bash profiles

echo -e "\033[1;33mLoading profiles ... \033[0m"


#------------------------------------------------
# Export PATHS
#------------------------------------------------
export DOTFILES=$HOME/personal/workspace/my-dotfiles
export PROFILE=$HOME/.profiles
export OLDCLASSPATH=$CLASSPATH

#------------------------------------------------
# PRIVATE AND PERSONAL SCRIPTS
#------------------------------------------------
test -d "${HOME}/.privatescripts" && export PATH=${HOME}/.privatescripts:${PATH}
test -d "${HOME}/.myscripts" &&  export PATH=${HOME}/.myscripts:${PATH}

#------------------------------------------------
# Source Profiles
#------------------------------------------------
test -e "${HOME}/.private_bashprofile" && source "${HOME}/.private_bashprofile"
test -e "${PROFILE}/default_profile" && source "${PROFILE}/default_profile"


echo -e "\033[1;33mwhich was last updated on\033[0m \033[1;31m$(stat -f "%Sm" $HOME/personal/workspace/my-dotfiles/.git/refs/heads/master)\033[0m"
echo -e "\033[1;33mLoaded ... \033[0m"
echo

# For iterm2
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
fact=`env | grep ITERM_PROFILE`
if [ "x${fact}" != "x" ]; then

    if [ "${fact}" != "ITERM_PROFILE=Default" ]; then
	    # The history is shared between my iTerm2 terminal tabs how can I switch that off
	    export HISTFILE=""
    fi
fi
