Environment setup
=========================================

This uses `dotfiles <https://github.com/jbernard/dotfiles>`_ 


**To replicate on new machine**

Install git first

Get the scripts from github project::

    cd ~
    git clone git@github.com:ashishrv/my-dotfiles.git Dotfiles

Get *dotfiles* utility::

	cd ~
	git clone https://github.com/jbernard/dotfiles dotfilecmd
	cd dotfilecmd
	bin/dotfiles --help

Now do a simple sync to restore various dot scripts::	

	bin/dotfiles --sync
	


	



