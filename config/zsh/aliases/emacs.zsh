#Emacs aliases

#Add emacs to our PATH (for doom)
export PATH=$PATH:$HOME/.config/emacs/bin

#all variables for emacs FLOW
if [ -f ~/personal.env ]; then 
	source ~/personal.env
fi

#alias to open emacs client ONLY if the daemon is running
function run_emacs() {
  if emacsclient -a false -e 't' &>/dev/null; then
    #First look at args provided and check to see which ones are flags
    local flag_count=0
    local nw_flag_provided=false

    for arg in "$@"; do
      #if the arg starts with a "-" (aka it is a flag)
      if [[ "$arg" == -* ]]; then
        flag_count=$((flag_count + 1))
        if [[ "$arg" == "-nw" ]]; then
          nw_flag_provided=true
        fi
      fi
    done

    #If -nw was not provided and no other flags were provided, pass the other args
    if [[ "$nw_flag_provided" == false && "$flag_count" -eq 0 ]]; then
      echo "Detected emacs server. Creating frame."

      #Open an emacsclient frame instead of a new emacs process 
      #for context, we should have the emacs daemon already running. this will just connect to the existing daemon
      #(INSANELY FASTER)
      #Run our emacs alias
      emacsclient --no-wait --create-frame $@
    #If -nw was provided and no other flags were provided, pass the other args
    elif [[ "$nw_flag_provided" == true && "$flag_count" -eq 1 ]]; then
      echo "Detected emacs server. Creating frame with '-nw'."
      emacsclient -nw --create-frame $@
    else 
      echo "Detected emacs server, but other flags were provided. Running 'emacs' with args"
      emacs $@
    fi
  else
    echo "No emacs server detected. Running regularly"

    #Run emacs with args
    emacs $@
  fi
}

#Kills emacs daemon
function kill_emacs() {
  emacsclient -e "(kill-emacs)"
}


#even more emacs aliases (TAKE THAT VIM!)
alias emacs="run_emacs"
alias e="run_emacs"
alias enw="run_emacs -nw"
