import tempfile
import os
import sys
from datetime import datetime
import time

# helper function for yes/no choices
def get_yes_or_no(prompt):
    while True:
        choice = input(f"{prompt} (y/n): ").strip().lower()
        if choice in ['y', 'n']:
            return choice


# takes in an arbitrary number of choices, prompts the user to pick a choice
# and then returns the value of the choice
def get_user_choice(prompt, *choices):
    # Print the list of choices with corresponding numbers
    print(f"{prompt}")
    for i, choice in enumerate(choices, 1):
        print(f"{i}: {choice}")

    while True:
        try:
            # Prompt the user for input and convert it to an integer
            choice_number = int(input("Enter the number of your choice: "))

            # Check if the entered number is a valid choice
            if 1 <= choice_number <= len(choices):
                return choices[choice_number - 1]  # Return the selected choice
            else:
                print("Invalid choice. Please enter a valid number.")
        except ValueError:
            print("Invalid input. Please enter a number.")

#Gets input via a tmp file
#This is useful for getting input for more complex text inputs such as Markdown
def getExternalInput(templateFile=None):

    #TODO: This flow only takes emacs-flow into mind, but this should be refactored to account for other inputs as well

    #0. Determine which type of way we want to get external input (vim, emacs-flow, none, etc)

    #1. Create a temporary file
    with tempfile.NamedTemporaryFile() as f:

        #TODO: this should be called if the script is exited early. That way we have a way to cleanup
        def cleanup():
            print("Force exiting")
            if (os.path.exists(f.name)):
                os.remove(f.name)
        
        #2. Write any necessary templates into the file
        if (templateFile != None):
            with open(templateFile) as template:
                print(templateFile)
                f.write(bytes(template.read(), 'utf-8'))
        f.seek(0) #go back to the start of the file

        #3. Save the current "last changed" metadata of the file
        initial_modification_time = os.path.getmtime(f.name)
        print(f"Last modified: {datetime.fromtimestamp(initial_modification_time)}")

        #4. Output the filename with a special formatting
        print("Now waiting on external editor. Please insert any keyboard input if you want to exit")
        print(f"[TMP FILE]:{f.name}")

        #5. Loop and compare the initial "last changed" to the current "last changed".
        while True:
            
            current_modification_time = os.path.getmtime(f.name)
            #5.1. Break once there is a difference
            if (initial_modification_time != current_modification_time):
                print(f"Detected change at {datetime.fromtimestamp(current_modification_time)}")
                break

            time.sleep(2) #sleep for 2 seconds before checking again

        #6. Return the ENTIRE content of the file as a long string
        content = f.read().decode('utf-8')
        return content
