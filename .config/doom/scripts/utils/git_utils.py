import os
import git
from urllib.parse import urlparse

GITHUB_BASE_NAME = os.environ["GITHUB_BASE_NAME"]

# finds the root git directory for a given directory
def find_git_root(directory):
    while directory:
        attempt = os.path.join(directory, '.git')
        if os.path.exists(attempt):
            return directory
        directory = os.path.dirname(directory)
    return None

#Given a directory, get the associated remote repository from github 
def get_remote_title_from_directory(directory):
    try: 
        repo = git.Repo(directory)
        remotes = repo.remotes
        github_remotes = [remote for remote in remotes if GITHUB_BASE_NAME in remote.url.lower() ]
        if github_remotes:
            github_url = github_remotes[0].url

            #Parse the github URL into components
            parsed_url_components = urlparse(github_url).path.strip('/').split('/')

            #Get our information from the components
            repo_owner = parsed_url_components[-2]
            repo_title = parsed_url_components[-1]
            if (repo_title.endswith('.git')):
                repo_title = repo_title[:-4]

            return f"{repo_owner}/{repo_title}"
        else:
            return None
    except git.InvalidGitRepositoryError:
        raise Exception(f"\"{directory}\" is Not a valid git repository!")
