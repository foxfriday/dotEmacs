SELF_DIR=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))

init:
	ln -sf $(SELF_DIR)early-init.el $(HOME)/.config/emacs/early-init.el
	ln -sf $(SELF_DIR)init.el $(HOME)/.config/emacs/init.el
	ln -sfn $(SELF_DIR)snippets $(HOME)/.config/emacs/snippets
