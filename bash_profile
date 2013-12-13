if [ -f /etc/profile ]; then
    PATH=""
    source /etc/profile
fi

# Set Prompt
export PS1="\[\e[1;32m\]\W \[\e[1;37m\]\[\e[0m\]$"

# Additional environment paths
if [[ "$PATH" != *"$HOME/.privatescripts"* ]]; then
	export PATH=$HOME/.privatescripts:$PATH
fi
if [[ "$PATH" != *"$HOME/.myscripts"* ]]; then
	export PATH=$HOME/.myscripts:$PATH
fi

# DOTFILES Directory
export DOTFILES=$HOME/Dotfiles

# PROFILES DIRECTORY
export PROFILE=$HOME/.profiles

# Keep oldclass paths if possible
export OLDCLASSPATH=$CLASSPATH

# Source private profile, which contains private paths
[[ -f "$HOME/.private_bashprofile" ]] && source "$HOME/.private_bashprofile"

# Source environment selector
#[[ -f "$PROFILE/environment_selector" ]] && source "$PROFILE/environment_selector"

# Additional information
[[ -f "$PROFILE/default_bashprofile" ]] && source "$PROFILE/default_bashprofile"

# Tmux remote terminal type support
alias ssh='TERM=xterm ssh'






