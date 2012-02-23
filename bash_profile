# Support for macports
export PATH=/opt/local/bin:$PATH

# Additional information
[[ -f "$HOME/.profiles/default_bashprofile" ]] && source "$HOME/.profiles/default_bashprofile"

# Keep oldclass paths if possible
export OLDCLASSPATH=$CLASSPATH

# This loads RVM into a shell session
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 










