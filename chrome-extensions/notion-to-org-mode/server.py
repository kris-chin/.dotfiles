from flask import Flask, request, jsonify
from flask_cors import CORS
import subprocess
import os

NOTION_TO_ORG_FILEPATH = os.path.expanduser("~/.local/bin/notion-to-org-mode/notion-to-org-mode")

app = Flask(__name__)
CORS(app)

#Calls the notion_to_org script
def call_script(payload):
    url = payload['url']

    #First, ensure that the url is a notion URL
    if ("notion.so" not in url):
        raise Exception("Not a Notion URL")
    
    #The page id is the last 32 characters of the url
    page_id = url[ len(url) - 32: ]

    #If the "Page_id" contains a hyphen in it, we grabbed too much, and this is most likely  not a valid page id
    if ("-" in page_id or "/" in page_id):
        raise Exception(f"'-' or '/' detected in '{page_id}'. This is not a page-id")

    #If we're here, our page_id is valid. So let's run the script!
    subprocess.run([NOTION_TO_ORG_FILEPATH, page_id])

    return jsonify({"pageid": page_id})

@app.errorhandler(Exception)
def internal_error(e):
    return jsonify({"error": f"{e}"}), 500

@app.route('/handle_click', methods=['POST'])
def handle_click():
    data = request.get_json();
    return call_script(data)

if __name__ == "__main__":
    app.run(port=5432)
