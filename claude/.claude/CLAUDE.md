
Avoid unneccessary permission prompts:
* Use echo "====" as a command separator instead of echo "----"
* Use && and || to chain commands, not newlines
* Don't use \; in find commands

When writing bash/python scripts: add a #! to the script, chmod it and run the script directly. Don't use patterns like `bash foo.sh` or `python foo.py`.

When correcting changes in existing commits: use git absorb or git commit
--amend to put the fixes into the matching commit.

Never write docstrings for private functions.

Only add docstrings when there's something important about the function's
behaviour to convey to callers which _isn't obvious from the function name and
arguments_.

When writing docstrings: Start with a one line summary of what the function
does, then explain the non-obvious behaviours. Don't include details of how
something works or why it was written that way. Don't explain arguments which
are obvious.

When you have a constant which is used in one place - put the declaration as
close as possible to where it is used. e.g. usually constants live inside the
function which uses them, not as separate top-level declarations.
