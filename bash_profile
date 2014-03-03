if [ -f /etc/profile ]; then
    PATH=""
    source /etc/profile
fi

# Some Colors first
# bash colors
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'
export CLICOLOR=1
export LSCOLORS=dxFxCxDxBxegedabagacad

#
# Some colors for the files
#
export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;37;41:mi=01;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;3
2:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=
01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:*.py=00;35:*.java=00;35"


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
[[ -f "$PROFILE/environment_selector" ]] && source "$PROFILE/environment_selector"

# Additional information
[[ -f "$PROFILE/default_bashprofile" ]] && source "$PROFILE/default_bashprofile"

# Tmux remote terminal type support
alias ssh='TERM=xterm ssh'






