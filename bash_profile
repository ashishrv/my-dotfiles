# Support for macports
export PATH=/opt/local/bin:$PATH

# DOTFILES Directory
export DOTFILES=$HOME/Dotfiles

# PROFILES DIRECTORY
export PROFILE=$DOTFILES/profiles

# Source private profile, which contains private paths
[[ -f "$HOME/.private_bashprofile" ]] && source "$HOME/.private_bashprofile"

# Additional information
[[ -f "$PROFILE/default_bashprofile" ]] && source "$PROFILE/default_bashprofile"

# Keep oldclass paths if possible
export OLDCLASSPATH=$CLASSPATH

# This loads RVM into a shell session
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 












