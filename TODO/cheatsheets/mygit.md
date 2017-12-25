Roi's GITsheet
==============


* `git init` # make a git repository with nothing
* `git remote add origin <name> path-to-repo`

* `git status --short -uno` (only files that git tracks)
* `git log` 
* `git diff` and to exclude `git diff ':(exclude)./package-lock.json'`
* `git apply`

## ***"Working Directory -> staged snapshot -> commit history"***

* `git add` # adds from working area

* `git commit -m "message"`

## reset:
* `git reset HEAD~"num"` # go back num commits
** `--soft` # move commits to staging snapshot
** `--mixed` # default, move commits to working directory
** `--hard` # remove all num commits completly
* `git reset HEAD~"num" "filepath"` - file only gets from staged snapshot to working directory
*can be used from commit and puts file both is staged snapshot and working directory*

**`git revert` is only used to make a new commit that reverts to a previous commit.**

## Checkout:
* `git checkout "branch_name"` # move branch
* `git checkout HEAD~"num"` "filepath" # checks out a new HEAD~"num" file
*if no num - kinda like svn revert -R "filepath" (also git reset --hard HEAD)*

## branch
* `git checkout -b`
* `git branch "name"`

## rebase
* `git rebase -i HEAD~<NUM>`# interactive - commit actions (like sqaush)
* `git rebase another` # to put our branch on top of another

## merge
* `git merge "branch_name"` # update branch (like master) from other branch
* `git merge "branch_from" "branch_to"`

## stash
* `git stash -` # to get all stash actions

## talk to repo and rebase
* `git pull` # is a git fetch than git merge on the current branch
* `git rebase <branch_name>` # same as before only takes the branche name changes (not updated to server). in order to update - git pull the branch name before
* `git push -u/-f origin <branch_name>`

## for justin
* `git fetch` # updates local info about server changes
* `git rebase origin/<branch_name>` # rabases the current branch changes on top of branch name server changes

*`vi ~/.gitconfig`*
