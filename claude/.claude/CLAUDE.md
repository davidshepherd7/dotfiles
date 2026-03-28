
Avoid unneccessary permission prompts:
* Use echo "====" as a command separator instead of echo "----"
* Use && and || to chain commands, not newlines
* Don't use \; in find commands


When writing bash/python scripts: add a #! to the script, chmod it and run the script directly. Don't use patterns like `bash foo.sh` or `python foo.py`.

When correcting changes in existing commits: use git absorb or git commit
--amend to put the fixes into the matching commit.

You can launch a terminal for the user with `claude-term.sh [DIR]`. If the user
says `t`: find the most recent relevatn directory and open a terminal in it,
default to your current working directory (i.e. `.`) if you are unsure.


You can open files for the user with `claude-editor.sh [FILE]`. If the user says
`e`: find the most recent relevant file and open it.
