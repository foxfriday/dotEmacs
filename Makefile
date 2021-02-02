SELF_DIR=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))

init:
	ln -s $(SELF_DIR)early-init.el $(HOME)/.emacs.d/early-init.el
	ln -s $(SELF_DIR)init.el $(HOME)/.emacs.d/init.el
	ln -s $(SELF_DIR)secrets.el $(HOME)/.emacs.d/secrets.el
	ln -s $(SELF_DIR)snippets $(HOME)/.emacs.d/snippets
