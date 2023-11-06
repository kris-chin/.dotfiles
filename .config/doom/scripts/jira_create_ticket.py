#!/usr/bin/env python3

# This script creates a new JIRA ticket and prints it to STDOUT.

from __future__ import annotations

from utils.jira_utils import get_user_data, get_custom_field_by_name, setup_jira
from utils.cli_utils import get_yes_or_no, get_user_choice
from utils.general_utils import readArrayEnvVar

import os
import sys
import argparse

# these projects are the ones we actually care about
# we will get the IDs of these projects later
RELEVANT_PROJECTS = readArrayEnvVar("RELEVANT_PROJECTS")
# names of product managers and qa engineers for use later
PRODUCT_MANAGERS = readArrayEnvVar("PRODUCT_MANAGERS")
QA_ENGINEERS = readArrayEnvVar("QA_ENGINEERS")
MYSELF = os.environ["MYSELF"]

# feel free to add more here
RELEVANT_COMPONENTS = readArrayEnvVar("RELEVANT_COMPONENTS")
RELEVANT_LABELS = readArrayEnvVar("RELEVANT_LABELS")

BASE_URL = os.environ["BASE_URL"]
DUMMY_ISSUE = os.environ["DUMMY_ISSUE"] #TODO: remove this

# TODO trycatch

def main(script_args=None):
    # setup argparse
    # TODO: everything should be passed as an arg (get a lot of args)
    parser = argparse.ArgumentParser()

    jira = setup_jira()

    myself = list(jira.search_users(MYSELF))[0]

    # there isn't an easy way to get the names of custom fields (we only get the id)
    # so we need to get all of the issues in the instance, and compare them to a dummy issue in order to get their names
    # raw() gives us all of the info that the jira server provides
    # TODO: account for if this is in a different JIRA project
    # TODO: consider turning these into some helpers for easier ways to get fields
    dummy_issue_key = DUMMY_ISSUE
    # normal_fields  = list(filter(lambda fieldName: "customfield" not in fieldName, dummy_issue.fields.__dir__()))

    ticketExists = get_yes_or_no("Does a ticket already exist?")

    if (ticketExists == 'y'):
        ticketName = input("Ticket Name: ")
        # TODO: Verify that ticket exists here
        issue = jira.issue(ticketName)
        print("printing...")

        sys.stdout.write(f"{issue.key}\n")

        # Print the status of the new issue
        sys.stdout.write(f"{issue.fields.status}\n")

        # Print a link for this issue
        sys.stdout.write(f"{BASE_URL}{issue.key}\n")

    elif (ticketExists == 'n'):
        # filter out all of our projects and only get the ones relevant to us
        # this is important because now we have all data associated with the project ouside of the keyword
        # (i think the api also takes in keys though? but i'm just doing this just to be safe)
        projects = list(filter(lambda project: project.key in RELEVANT_PROJECTS, jira.projects()))
        project = get_user_choice("📕 Choose a Project:", *projects)
        summary = input("📝 Write summary: ")
        description = input("📝 Write description: ")
        issueTypes = jira.issue_types_for_project(project.id)
        issueType = get_user_choice("Choose an issue type:", *issueTypes)
        projectComponents = jira.project_components(project.id)
        relevantComponents = list(filter(lambda component: component.name in RELEVANT_COMPONENTS, projectComponents))
        priority = get_user_choice("Priority: ", *jira.priorities())

        # TODO: wrapping these in lists for now, but ideally, we should be able to select multiples of these
        components = [get_user_choice("Choose component: ", *relevantComponents)]
        labels = [get_user_choice("Choose labels: ", *RELEVANT_LABELS)]

        # this project has the QA assignee and PM assignee as mandatory, so we need those as well
        pmFullName = get_user_choice("PM Assignee: ", *PRODUCT_MANAGERS)
        pm = get_user_data(jira, pmFullName)
        if (pm is None):
            print("Couldn't find pm")
            return
        qaFullName = get_user_choice("QA Assignee: ", *QA_ENGINEERS)
        qa = get_user_data(jira, qaFullName)
        if (qa is None):
            print("Couldn't find qa")
            return

        # we need the ids of all of the custom field names
        pm_field_name = get_custom_field_by_name(jira, dummy_issue_key, "PM Assignee")[0]['id']
        qa_field_name = get_custom_field_by_name(jira, dummy_issue_key, "QA Assignee")[0]['id']

        issue_dict = {
            'project': project.raw,
            'summary': summary,
            'description': description,
            'issuetype': issueType.raw,
            'assignee': myself.raw,
            'labels': labels,
            'priority': priority.raw,
            'components': [component.raw for component in components],
            pm_field_name: pm.raw,
            qa_field_name: qa.raw
        }
        new_issue = jira.create_issue(fields=issue_dict)

        # Print to stdout the name of the ticket
        sys.stdout.write(f"{new_issue.key}\n")

        # Print the status of the new issue
        sys.stdout.write(f"{new_issue.fields.status}\n")

        # Print a link for this issue
        sys.stdout.write(f"{BASE_URL}{new_issue.key}\n")

if __name__ == "__main__":
    main()
