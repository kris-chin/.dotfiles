import os
from github import Github, Auth

GITHUB_BASE_NAME = os.environ["GITHUB_BASE_NAME"]
GITHUB_API_TOKEN = os.environ["GITHUB_API_TOKEN"].rstrip() #remove whitespace at end of token (should it be "API" or "ACCESS"?)

def setup_github():
    auth = Auth.Token(GITHUB_API_TOKEN)
    g = Github(auth=auth, base_url=f"https://{GITHUB_BASE_NAME}/api/v3")
    return g

#manually filters open pull requests by user (I can't seem to query it via the API)
def filter_open_pulls_by_user(repoObject, username):
    pulls = repoObject.get_pulls(state="open")

    def pr_is_made_by_user(pr):
        return pr.user.login == username

    return list(filter(pr_is_made_by_user, pulls))

#manually filters open pull requests by branch name (Couldn't seem to query this via the API either)
def filter_open_pulls_by_branch_name(repoObject, branch_name):
    pulls = repoObject.get_pulls(state="open")

    if (branch_name == None):
        print("Warning: No Branch Name was provided")
        return []

    def pr_has_branch_name(pr):
        #So, label of the head actually prioritizes the organization over the username (from my observation?)
        #ie. if {username} belongs to {organization}, their head will say {organization}:branch_name instead of {user}:branch_name
        #so, i'll just filter by if the label string CONTAINS the branch name, instead of an exact match
        return pr.head.label.find(branch_name) != -1

    return list(filter(pr_has_branch_name, pulls))

#Get branch head from local head
def get_branch_head_by_branch_name(repoObject, branch_name):
    #This may seem redundant, but I have a strange feeling that I may need to modify this function later to account for head refs. but it seems like I don't have to for now
    branch = repoObject.get_branch(branch=branch_name)
    return branch.name
