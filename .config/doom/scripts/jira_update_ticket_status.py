#!/usr/bin/env python3

from utils.jira_utils import setup_jira, get_custom_field_by_name
from utils.cli_utils import get_user_choice, get_yes_or_no
from utils.general_utils import DictToObject
import sys
import argparse
from datetime import date

# moves forward a ticket status

QA_STATUS_NAME = "QA In Progress"


# helper class that prints a given key instead when printed
class PrintPropertyDict(dict):
    def __init__(self, dictionary, key):
        super().__init__(dictionary)
        self.key = key

    def __str__(self):
        if self.key in self:
            return str(self[self.key])
        else:
            return None


def main(script_args=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("issueKey", help="the name of the key")
    parser.add_argument("--prIsMerged", help="is the pr merged?", action="store_true")
    args = DictToObject(**script_args) if (script_args != None) else parser.parse_args()

    jira = setup_jira()

    # 1. Get issuekey from args
    print("Editing: " + str(args.issueKey))
    issue = jira.issue(args.issueKey)

    # 2. Find Transitions
    transitions = [PrintPropertyDict(x, "name") for x in jira.transitions(issue)]

    # 3. Choose transition and get information about it
    chosenTransition = get_user_choice("Choose transition: ", *transitions)
    nextStatus = chosenTransition['to']
    transitionId = chosenTransition['id']

    # Block QA in progress transition unless PR has been merged already
    if (nextStatus['name'] == QA_STATUS_NAME):
        # Was PR created + merged?
        if args.prIsMerged:
            #TODO: Generalize this for other people
            #Get Test Details
            testDetails = input("Test details: ")

            #Get PPV Details
            if (get_yes_or_no("Should PPV Details be the same?") == "y"):
                ppvDetails = testDetails
            else:
                ppvDetails = input("PPV details: ")

            #Get Fix Version
            #TODO: Generalize the fix version acquisition (not everyone is in project)

            #1. Get current date
            #2. Look through existing fix versions for date
            #3. If there's a match, prompt if this is the correct date
            #4. Set Fix Version

            #Call updateIssue with these new properties
            print("pr is merged! continuing!")
            return
        else:
            print("Moving to QA is blocked until a PR is both created and merged")
            return

    # 4. Transition the issue
    jira.transition_issue(issue, transitionId)

    # 5. Print new transition name in stdout
    sys.stdout.write(f"{nextStatus['name']}\n")


if __name__ == "__main__":
    main()
