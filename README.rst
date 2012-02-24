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

Backup existing dotfiles::

    cd ~; mkdir -p dotbackup; 
    mv .bash_profile dotbackup/; mv .gitconfig dotbackup/; mv .hgrc  dotbackup/;
    

Now do a simple sync to restore various dot scripts::	

	dotfilecmd/bin/dotfiles --sync
	

Post sync, you will have all the shell utilities and shortcuts::


     ~/
       |
       |--dotfilecmd/
       |       |------bin/
       |               |----dotfiles
       |
       |-- Dotfiles/
       |      |
       |      |----profiles/
       |      |            :
       |      |------------:---myscripts/
       |      |            :...      :
       |      |---------------:------:-------privatescripts/
       |      |--bash_profile :      :               :
       |           :          :      : symlinks      :
       |--.bash_profile       :      :               :
       |-----------------.profiles/  :               :
       |---------------------------.myscripts/       :
       |---------------------------------------.privatescripts/
	



