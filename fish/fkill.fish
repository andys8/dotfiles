function fkill -d "Kill processes with fzf"
  eval "ps aux | grep $USER | fzf --header (ps aux | head -1) --query (commandline)" | read select

  if not test -z $select
    eval "echo -n \"$select\" | awk '{ print \$2 }'" | read pid

    if kill -0 $pid
      kill -9 $pid
    end
  end
end
