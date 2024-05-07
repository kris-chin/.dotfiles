#!/usr/bin/env python3
import argparse
import os
import git
from utils.cli_utils import get_yes_or_no
from utils.general_utils import DictToObject
import logging
import sys
sys.path.append('~/.config/doom/scripts')
from git_switch_to_branch import main as switch_to_branch
from github_create_pr import main as create_pr

#TODO: variably choose the remote

def main(script_args=None):
    #NOTE: for git feedback, logging MUST be enabled
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser()
    parser.add_argument("--dir", help="Directory which to run from", type=str)
    parser.add_argument("--branch_name", help="Push to remote for a certain branch", type=str)
    args = DictToObject(**script_args) if (script_args != None) else parser.parse_args()

    directory = args.dir if (args.dir != None) else os.getcwd()
    print("Attempting to push from " +str(directory))

    repo = git.Repo(directory, search_parent_directories=True)
    current_branch = repo.active_branch

    #0. Switch to a desired branch if necessary
    if ( (args.branch_name != None) and (str(current_branch) != args.branch_name) ):
        print(f"Detected switch to '{args.branch_name}'. Running switch_flow...")
        #Switch to the branch
        switch_to_branch({"dir": directory, "target_branch": args.branch_name})
        print("END SWITCH FLOW")
        #Get a pointer to the new current branch
        current_branch = repo.active_branch
        print("Switched to " + str(current_branch))

    args = ["origin", current_branch]

    #Does remote even exist at all?
    remote_branch_exists = any(remote_ref.remote_head == str(current_branch) for remote_ref in repo.remote().refs)

    #1. Determine if we need a force push or a regular push
    if (remote_branch_exists):
        local_commit = current_branch.commit
        remote_commit = repo.remotes.origin.refs[str(current_branch)].commit

        if (local_commit != remote_commit):
            print("Detected that a force push is needed. Will force.")
            args.append("--force")

    #2. Attempt a Push
    print("Attempting to push...\n(I can't get NPM console logs to show up here yet, so please be patient)")
    repo.git.push(*args)
    print("Successfully Pushed")
    
    #3. If the Push was successful, prompt to ask if a PR should be made
    if (get_yes_or_no("Should we make a PR for this branch?") == "y"):
        print("well too bad, im not done yet")
        #3. If a PR needs to be made, run create_PR flow and add PR link as heading property
        #create_pr({})

    #TODO: This won't fly if we need to make a PR, since we actually need input from the script
    print("Exiting...")

if __name__ == "__main__":
    main()
