all:
		emacs -batch -l ~/.emacs.d/init.el --eval "(run-hooks 'emacs-startup-hook)" -l marine.el -f ert-run-tests-batch-and-exit
