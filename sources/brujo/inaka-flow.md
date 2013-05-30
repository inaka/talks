![Inaka](http://inaka.net/assets/img/inaka-logo.png "Inaka")
# Inaka's Work Flow
## _…or how do we get stuff done around here_

### Introduction
This document describes the usual workflow for Inaka's projects.
It's meant to be followed by all developers and in every project at the firm.
If you're a dev and you find a project with something that doesn't quite fit in this workflow description, please contact [@elbrujohalcon](/elbrujohalcon) and improve this document together.

### Inaka's Project
#### Description
For this kind of project we generally have:
* a github repo under inaka's account, e.g. http://github.com/inaka/the_project
* a hipchat room, e.g. "The Project"
* a pivotal project (e.g. "The Project") or a list of issues in github (e.g. http://github.com/inaka/the_project/issues)
* a freckle project, e.g. "The Project"
* a group of developers
If this setting doesn't match your current project structure, skip this section… you'll find what you're looking for below in this document :)

#### Workflow
Let's say you're _Bob_ and you work on _The Project_. You need to implement _The Task_ which has #333 (either in Pivotal or Github issues).
This is what you should do…
+ Open the pivotal story / github issue _#333_ and read it carefully
    + if it's a piv story mark it as started by pressing the "Start" button
    + if you have further comments or you want to discuss this with the PM or whatever, go to _The Project_ hipchat room and talk
      **NOTE:** Don't do it in private, share your knowledge and your doubts, Bob! We're here to help you!
+ Go to your console and make sure you're up to date with the project
```bash
$ cd /path/to/the_project
$ git checkout master
$ git fetch origin --prune
$ git merge origin/master
$ git push
```
+ Create a branch named <username>.brief.description.of.your.task (you can optionally include the story/issue number).
```bash
$ git checkout -b bob.333.this.task
$ git push --set-upstream origin bob.333.this.task
```
+ You should see a message on hipchat telling everybody you created that branch
    + if you don't see it, just tell them yourself :P

+ Do your magic :star2:.
    + Commit and push as often as you can (at least once a day)
    + Always include the story/issue number on your commits:
      ```bash
$ git add <the files you changed>
$ git commit -m "[#333] Added test cases for this new feature"
$ git push origin bob.333.this.task
      ```

+ Mark the pivotal story as finished ("Finished" button)

+ Go to github, open a pull request from the new branch.

+ Mark the pivotal story as delivered ("Deliver" button).

+ Record your time in freckle, with the story/issue number and a brief description of what you did.

+ If your PR was rejected or you need to change it somehow, go back to your local branch:
    git reset <sha of the last commit before your changes>
    git add <files>
    git commit
    git push -f origin marcelog_sample_story_123456789
This will allow you to clean up your commit history for the new PR.

+ Change again to master, pull new changes and update repo:
    git checkout master && git pull --rebase origin master && git fetch

+ On conflicts, try to merge (should not have to do this.. )
  git mergetool

+ delete your branch (AFTER someone merges it):
git branch -d marcelog_sample_story_123456789
git push origin :marcelog_sample_story_123456789
