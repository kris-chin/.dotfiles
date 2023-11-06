import os

# finds the root git directory for a given directory
def find_git_root(directory):
    while directory:
        attempt = os.path.join(directory, '.git')
        if os.path.exists(attempt):
            return directory
        directory = os.path.dirname(directory)
    return None
