((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-commit nil
	       ("--all"))
 (magit-diff
  ("--no-ext-diff" "--stat")
  ("--no-ext-diff"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-file-dispatch nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n10"))
 (magit-log:-n "100" "10")
 (magit-merge nil)
 (magit-pull nil)
 (magit-push nil
	     ("--force-with-lease")
	     ("--force"))
 (magit-reset nil)
 (magit-stash nil)
 (magit-worktree nil))
