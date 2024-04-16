#!/usr/bin/env python3
import argparse
from utils.general_utils import DictToObject
from utils.cli_utils import getExternalInput
from utils.github_utils import setup_github, filter_open_pulls_by_branch_name, get_branch_head_by_branch_name
from utils.git_utils import get_remote_title_from_directory
import logging
import os
import sys

def PrAlreadyExists(repoObject, branch_name):
    #1. Query GitHub for Pull Requests by user
    pulls = filter_open_pulls_by_branch_name(repoObject, branch_name)
    #2. If Pulls contains a branch, print the existing PR number and return
    if (len(pulls) > 0):
        print("PR already exists: printing information..")
        pr = pulls[0]
        sys.stdout.write(f"{pr.number}\n")
        sys.stdout.write(f"{pr.html_url}\n")
        return True
    else:
        return False

def SetUpGitLogs():
    logging.basicConfig(level=logging.INFO)

def GetArguments(script_args=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--branch_name", help="Name of the branch to create PR from")
    parser.add_argument("--jira_issue", type=str, help="Jira Issue ID")
    parser.add_argument("--text_editor", type=str, help="Text Editor to use for editing PR content")
    parser.add_argument("--dir", type=str, help="If defined, look from this directory")

    args = DictToObject(**script_args) if (script_args != None) else parser.parse_args()
    return args

TEMPLATE_FILE = os.path.expanduser("~/.config/doom/scripts/templates/pull_request.md")

#Context: This is called already after changes have been pushed to a remote
def main(script_args=None):
    SetUpGitLogs()
    args = GetArguments(script_args)

    github = setup_github()

    if (args.jira_issue == None):
        print("Warning: Creating PR without associated JIRA ticket. Useful Information will be missing.")
    
    #Get the GitHub repo associated with the dir
    repo_title = get_remote_title_from_directory(args.dir)
    if (repo_title == None):
        raise Exception("Couldn't find an associated Remote with this dir!")

    repo = github.get_repo(repo_title)

    if (args.branch_name == None):
        print("Warning: no branch name")

    #1. Check if a PR already exists, print it to STDOUT and exit
    #This is to avoid redundant PRs
    if (PrAlreadyExists(repo, args.branch_name)):
        #The conditional already printed to STDOUT, so just exit.
        return

    #If we are here, the PR doesn't exist, so just continue

    if (args.text_editor == None):
        raise Exception("Sorry! A text editor is required (I should change this)")

    if (args.text_editor != "emacs-flow"):
        raise Exception(f"Sorry! I only wrote support for emacs-flow (my own mode). What was passed in: \"{args.text_editor}\"")
    
    #Fuck it, I'm assuming we are using emacs-flow here, lets just get the ENTIRE PR data in here
    pr_data = getExternalInput(TEMPLATE_FILE)

    #Read the PR data and extract our PR information from it
    pr_lines = pr_data.split('\n', 1)

    #Get the first line and set it as title
    title = pr_lines[0]
    #TODO: Get JIRA information and add it to PR

    #Get the rest of the content and set it as the body
    body = pr_lines[1] if len(pr_lines) > 1 else ''

    branch_head = get_branch_head_by_branch_name(repo, args.branch_name)

    #Create the PR and get the number
    pr = repo.create_pull(title=title, body=body, base="main", head=branch_head)

    #Print the PR number
    sys.stdout.write(f"{pr.number}\n")
    sys.stdout.write(f"{pr.html_url}\n")

if __name__ == "__main__":
    main()
