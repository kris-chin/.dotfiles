import jenkins
import os

JENKINS_BASE_URL = os.environ["JENKINS_BASE_URL"]
JENKINS_API_TOKEN = os.environ["JENKINS_API_TOKEN"]
USER_EMAIL = os.environ["USER_EMAIL"]

def setup_jenkins():
    server = jenkins.Jenkins(JENKINS_BASE_URL, username=USER_EMAIL, password=JENKINS_API_TOKEN)
    version = server.get_version()
    print("Version: " + str(version))
