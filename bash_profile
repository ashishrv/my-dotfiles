#------------------------------------------------
# BASH PROFILE - Entry Point
# https://news.ycombinator.com/item?id=18898523
#------------------------------------------------


#------------------------------------------------
# Load everything from bashrc
#------------------------------------------------
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export PATH="$HOME/.poetry/bin:$PATH"
