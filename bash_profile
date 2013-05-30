# Support for macports
export PATH=/opt/local/bin:/opt/local/sbin:$PATH:$HOME/.privatescripts:$HOME/.myscripts

# DOTFILES Directory
export DOTFILES=$HOME/Dotfiles

# PROFILES DIRECTORY
export PROFILE=$HOME/.profiles

# Source private profile, which contains private paths
[[ -f "$HOME/.private_bashprofile" ]] && source "$HOME/.private_bashprofile"

# Additional information
[[ -f "$PROFILE/default_bashprofile" ]] && source "$PROFILE/default_bashprofile"

# Keep oldclass paths if possible
export OLDCLASSPATH=$CLASSPATH

# This loads RVM into a shell session
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

##
# Your previous /Users/ashish/.bash_profile file was backed up as /Users/ashish/.bash_profile.macports-saved_2012-09-20_at_13:40:36
##

# MacPorts Installer addition on 2012-09-20_at_13:40:36: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin
# Finished adapting your PATH environment variable for use with MacPorts.

