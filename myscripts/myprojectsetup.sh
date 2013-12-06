##############################
##
## Script Driver Headers
## 
##############################

# Basic script paths
curdir=`pwd`
scriptpath=`dirname $0`

#
# Yes / No prompt facility
#
promptyn () {
    while true; do
        read -p "$1 " yn
        case $yn in
            [Yy]* ) return 0;;
            [Nn]* ) return 1;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}

#
# Construct absolute path from given path and a relative path
#
get_relative_path () { 
	given_path=$1
  	relative_path=$2
  	relative_path_fragment=''
  	for c in  `echo $relative_path |  tr "/" " "`; do
		if [ "$c" ==  '..' ]; then
			given_path=`dirname $given_path`
		else
			if [ "$relative_path_fragment" ==  '' ]; then
				if [ "$c" !=  '.' ]; then
					relative_path_fragment=$c
				fi
			else
				if [ "$c" !=  '.' ]; then
					relative_path_fragment=$relative_path_fragment'/'$c
				fi
			fi
		fi
	done
	if [ "$relative_path_fragment" !=  '' ]; then
		script_path_abs=$given_path'/'$relative_path_fragment
	else
		script_path_abs=$given_path
	fi
	echo $script_path_abs
}

if [ -z "$1" ]; then
	echo "Provide a relative path"
	exit 1
else
	script_abs_dirpath=$( get_relative_path  $curdir $scriptpath)
	application_abs_dirpath=$( get_relative_path  $curdir $1)
	echo "Path for project directory: " $1
fi

#
# Get project name
#
get_projectname () { 
	read -r -p 'Please enter the project name >>> ' projectname
	echo $projectname
}

#
# Gather project details
#
project_name=$( get_projectname )
echo "APP DIRECTORY: "$application_abs_dirpath


##################### Setup the project environment ##############

APP_PROJECTHOME=$application_abs_dirpath

#
# bin directory 
#

mkdir -p $APP_PROJECTHOME/bin
rm -f $APP_PROJECTHOME/bin/appenv.sh
mkdir -p $APP_PROJECTHOME/src
mkdir -p $APP_PROJECTHOME/src/bin

# Define your sourcing env.sh template Headers
cat >$APP_PROJECTHOME/bin/appenv.sh << EOF
#!/bin/sh
called=\$_
[[ \$called != \$0 ]] && echo "Script sourced" || echo "Running script"
echo "\\\$BASH_SOURCE \${BASH_SOURCE[@]}"

export APP_PROJECTHOME=$application_abs_dirpath

alias cdproj='cd \$APP_PROJECTHOME'

if [ -z "\$IN_APP_ENVIRONMENT" ]; then
	export IN_APP_ENVIRONMENT="yes"
else
	echo "Already in app environment, please deactivate first!"
	return;
fi

# Store previous prompts
if [ -z "\$OLD_PS1" ]; then
	export OLD_PS1=\$PS1
	export PS1="\[\e[1;32m\]\W \[\e[1;37m\]\[\e[0m\]($project_name)$"
fi

if [ -z "\$OLD_PATH" ]; then
	export OLD_PATH=\$PATH
	export PATH=\$APP_PROJECTHOME/src/bin:\$APP_PROJECTHOME/bin:\$OLD_PATH
fi

deactivate_binpath () { 
	[[ -z "\$IN_APP_ENVIRONMENT" ]] || unset IN_APP_ENVIRONMENT
	[[ -z "\$OLD_PS1" ]] || export PS1=\$OLD_PS1
	[[ -z "\$OLD_PS1" ]] || unset OLD_PS1
	[[ -z "\$OLD_PATH" ]] || export PATH=\$OLD_PATH
	[[ -z "\$OLD_PATH" ]] || unset OLD_PATH
}

EOF


#
# Python library
#

if promptyn "Do you want to add python lib directory?"; then
	mkdir -p $APP_PROJECTHOME/src/lib/ext
	mkdir -p $APP_PROJECTHOME/src/lib/int
cat >>$APP_PROJECTHOME/bin/appenv.sh << EOF
if [ -z "\$SET_APP_PYTHONPATH" ]; then
	export OLD_PYTHONPATH=\$PYTHONPATH
	export PYTHONPATH=\$APP_PROJECTHOME/src/lib/int:\$APP_PROJECTHOME/src/lib/ext:\$OLD_PYTHONPATH
fi

deactivate_pythonenv () { 
	export PYTHONPATH=\$OLD_PYTHONPATH
	unset OLD_PYTHONPATH
}
EOF

else
    echo "Skipped ..."
fi


#
# Configuration directory
#

if promptyn "Do you want to create config directory?"; then
    mkdir -p $APP_PROJECTHOME/src/conf
cat >>$APP_PROJECTHOME/bin/appenv.sh << EOF

export APP_CONF_PATH=\$APP_PROJECTHOME/src/conf
[[ \$1 == '' ]] && export APPENV='local' || export APPENV=\$1
[[ \$1 == '' ]] && export APPCONF='appconf_local.conf' || export APPCONF="appconf_\$1.conf"

deactivate_confenv () { 
	unset APPCONF
	unset APPENV
	unset APP_CONF_PATH
}
EOF

else
    echo "Skipped ..."
fi


#
# Supervisor support
#

if promptyn "Do you want supervisord directory?"; then
    SUPERVISOR_PATH=$APP_PROJECTHOME/src/supervisor
    mkdir -p $SUPERVISOR_PATH
	mkdir -p $SUPERVISOR_PATH/appconf
	mkdir -p $SUPERVISOR_PATH/history
	mkdir -p $SUPERVISOR_PATH/pid
	mkdir -p $SUPERVISOR_PATH/tmp
else
    echo "Skipped ..."
fi


#
# Log directory
#

if promptyn "Do you want log directory?"; then
    mkdir -p $APP_PROJECTHOME/src/logs/childlogs
else
    echo "Continuing ..."
fi


#
# Clean log directory
#

rm -f $APP_PROJECTHOME/src/logs/childlogs/*.log



# Define your sourcing env.sh template Headers
cat >>$APP_PROJECTHOME/bin/appenv.sh << EOF

deactivate () { 
	if [ "\`type -t deactivate_binpath\`" = 'function' ]; then
    	deactivate_binpath
    	unset deactivate_binpath
	fi
	if [ "\`type -t deactivate_pythonenv\`" = 'function' ]; then
    	deactivate_pythonenv
    	unset deactivate_pythonenv
	fi
	if [ "\`type -t deactivate_confenv\`" = 'function' ]; then
    	deactivate_confenv
    	unset deactivate_confenv
	fi
	unset deactivate
}

echo 
echo "APP HOME: " \$APP_PROJECTHOME
echo "APP BIN PATH" \$APP_PROJECTHOME/bin
echo "APP BIN: " \$APP_PROJECTHOME/src/bin
echo "APP CONF PATH: " \$APP_CONF_PATH
echo "APP ENV: " \$APPENV
echo "APP CONF: " \$APPCONF
EOF

echo "Application environment script created at: " $APP_PROJECTHOME/bin/appenv.sh


