Environment setup
=========================================

This uses `dotfiles <https://github.com/jbernard/dotfiles>`_ 


**To replicate on new machine**

Install git first

Get the scripts from github project::

    cd ~
    git clone git://github.com/ashishrv/my-dotfiles.git Dotfiles

Get *dotfiles* utility::

	cd ~
	git clone https://github.com/jbernard/dotfiles dotfilecmd
	dotfilecmd/bin/dotfiles --help

Now do a simple sync to restore various dot scripts::	

	dotfilecmd/bin/dotfiles --sync
	

Post sync, you will have all the shell utilities and shortcuts::

        ~/
          |
          |-dotfilecmd/
          |       |------ bin/
          |                |---- dotfiles
          |-.bash_profile .......
          |-.profiles"""""""""""""""""""""""""
          |                     .            "
          |---- Dotfiles/       .  symlinks  "
                    |           .            "
                    |------ bash_profile     "
                    |                        "
                    |---------------------profiles/
                    |

	



