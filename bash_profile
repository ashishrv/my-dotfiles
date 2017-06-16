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

function title {
    echo -ne "\033]0;"$*"\007"
}


# SET PROPMT
symbol="⚡ "
export PS1='$symbol\[\e[36;1m\]\W\[\e[0m\] $(__git_ps1 "[\[\e[0;32m\]%s\[\e[0m\]\[\e[0;33m\]$(parse_git_dirty)\[\e[0m\]]")\$ \[\e[0m\]'
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"


# Bash completion
# brew install bash-completion
# brew tap homebrew/completions
# ln -s /Applications/Docker.app/Contents/Resources/etc/docker.bash-completion 
# ln -s /Applications/Docker.app/Contents/Resources/etc/docker-machine.bash-completion 
# ln -s /Applications/Docker.app/Contents/Resources/etc/docker-compose.bash-completion 

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Bash prompt
test -e "${PROFILE}/util_prompt" && source "${PROFILE}/util_prompt"

# Hook up direnv
# brew install direnv
eval "$(direnv hook bash)"

# Hook up fzf
# brew install fzf
# Install shell extension: /usr/local/opt/fzf/install
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'





