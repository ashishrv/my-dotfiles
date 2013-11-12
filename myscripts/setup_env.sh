##############################
##
## Script Driver Headers
## 
##############################

# Basic script paths
curdir=`pwd`
scriptpath=`dirname $0`

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
	script_abs_dirpath=$( get_relative_path  $curdir $scriptpath)
	application_abs_dirpath=$( get_relative_path  $script_abs_dirpath '..')
else
	script_abs_dirpath=$( get_relative_path  $curdir $scriptpath)
	application_abs_dirpath=$( get_relative_path  $curdir $1)
	echo "Path for project directory: " $1
fi

echo "PROJECT BUILDER: "$script_abs_dirpath/'setup_env.conf'
echo "APP DIRECTORY: "$application_abs_dirpath


PY_PATH1=$application_abs_dirpath/lib/ext
PY_PATH2=$application_abs_dirpath/lib/int
APP_BIN_PATH=$application_abs_dirpath/bin
APP_CONF_PATH=$application_abs_dirpath/conf
SUPERVISOR_PATH=$application_abs_dirpath/supervisor
LOG_PATH=$application_abs_dirpath/logs


read -r -p 'Please enter the project name >>> ' project_name
echo $project_name

# Create the bin directory, if required
mkdir -p $PY_PATH1
mkdir -p $PY_PATH2
mkdir -p $APP_BIN_PATH
mkdir -p $APP_CONF_PATH
mkdir -p $SUPERVISOR_PATH
mkdir -p $SUPERVISOR_PATH/appconf
mkdir -p $SUPERVISOR_PATH/history
mkdir -p $SUPERVISOR_PATH/pid
mkdir -p $SUPERVISOR_PATH/tmp
mkdir -p $LOG_PATH/childlogs

# Clean application env file
rm -f $APP_BIN_PATH/appenv.sh
rm -f $LOG_PATH/childlogs/*.log


# Define your sourcing env.sh template Headers
cat >$APP_BIN_PATH/appenv.sh << EOF
#!/bin/sh
called=\$_
[[ \$called != \$0 ]] && echo "Script sourced" || echo "Running script"
echo "\\\$BASH_SOURCE \${BASH_SOURCE[@]}"

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

if [ -z "\$SET_APP_PYTHONPATH" ]; then
	export OLD_PYTHONPATH=\$PYTHONPATH
	export SET_APP_PYTHONPATH='yes'
	export PYTHONPATH=$PY_PATH1:$PY_PATH2:\$OLD_PYTHONPATH
fi
if [ -z "\$OLD_PATH" ]; then
	export OLD_PATH=\$PATH
	export PATH=$APP_BIN_PATH:\$OLD_PATH
	export APPBIN=$APP_BIN_PATH
fi

deactivate () { 
	[[ -z "\$IN_APP_ENVIRONMENT" ]] || unset IN_APP_ENVIRONMENT
	[[ -z "\$OLD_PS1" ]] || export PS1=\$OLD_PS1
	[[ -z "\$OLD_PS1" ]] || unset OLD_PS1
	[[ -z "\$OLD_PATH" ]] || export PATH=\$OLD_PATH
	[[ -z "\$OLD_PATH" ]] || unset OLD_PATH
	export PYTHONPATH=\$OLD_PYTHONPATH
	unset OLD_PYTHONPATH
	unset SET_APP_PYTHONPATH
	unset APPBIN
	unset APPCONF
	unset APPENV
}

[[ \$1 == '' ]] && export APPENV='local' || export APPENV=\$1
[[ \$1 == '' ]] && export APPCONF='appconf_local.conf' || export APPCONF="appconf_\$1.conf"
echo 
echo "APPENV: " \$APPENV
echo "APPCONF: " \$APPCONF
echo "APPBIN: " \$APPBIN
EOF

echo "Application environment script created at: " $APP_BIN_PATH/appenv.sh


