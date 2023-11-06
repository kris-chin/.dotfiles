import os
from jira import JIRA
import requests
import base64
import sys

JIRA_BASE_URL = os.environ["JIRA_BASE_URL"]
JIRA_API_TOKEN = os.environ["JIRA_API_TOKEN"].rstrip() #remove whitespace at end of token
USER_EMAIL = os.environ["USER_EMAIL"]

# helper function to quickly get username based on full name
def get_user_data(jira, fullName):
    search = jira.search_users(fullName)
    if (len(search) == 1):
        return list(search)[0]
    else:
        return None


# goes through a list of custom fields and returns the one with the matching name
# returns a list (just in case multiple fields have the same name)
def get_custom_field_by_name(jira, dummy_issue_key, name):
    dummy_issue = jira.issue(dummy_issue_key)
    customFields = list(filter(lambda field: field['id'] in dummy_issue.raw['fields'], jira.fields()))
    return list(filter(lambda customField: name in customField['name'], customFields))


# sets up jira and returns a jira instance
def setup_jira():
    # authenticate jira
    jira = JIRA(
            token_auth=JIRA_API_TOKEN,
            server=JIRA_BASE_URL
    )
    return jira

# manually makes http request to JIRA api
def get_from_jira(url):
    headers = {
        'authorization': f'Bearer {JIRA_API_TOKEN}',
        'content-type': 'application/json'
    }
    response = requests.get(f'{JIRA_BASE_URL}{url}', headers=headers)
    if not response.status_code == 200:
        error = f'get_From_jira() was not successful: {response.status_code} {response.reason}'
        raise RuntimeError(error)

    if response is None:
        raise Exception("response returned none")
    return response.json()
