function killport
    if test -n "$argv"
        set port $argv[1]
        set pids (lsof -ti :$port)
        if test -z "$pids"
            echo No process with port $port
        else
            for pid in $pids
                kill $pid
                echo Killed process with pid $pid and port $port
            end
        end
    else
        echo "Usage: killport <port>"
    end
end
