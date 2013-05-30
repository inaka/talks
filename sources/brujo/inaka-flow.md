+ Select a pivotal story to work on, mark it as started ("Start" button).

+ Go to the freckle project for that story/project, start the timer.

+ Create a branch <username>_descriptive_stuff_ptstorynumber, switch to it.
  git checkout -b marcelog_sample_story_123456789

+ Go to the hipchat for that project, ask any questions you might need, give feedback, tell other about what you're about to do.

+ Do your thing.

+ Optional: cleanup your commits:
    git rebase -i master

+ Push your changes to the new branch.
  git push origin marcelog_sample_story_123456789

+ Mark the pivotal story as finished ("Finished" button)

+ Go to github, open a pull request from the new branch.

+ Mark the pivotal story as delivered ("Deliver" button).

+ Go to freckl, click on "log" and write a brief description of what did you do, including #<pt story number>.

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
