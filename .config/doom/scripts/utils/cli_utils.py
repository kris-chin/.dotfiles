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
