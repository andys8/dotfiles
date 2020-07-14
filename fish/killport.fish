function killport
    if test -n "$argv"
        set port $argv[1]
        set pids (lsof -ti :$port)
        if test -z "$pids"
            echo No process with port $port
        else
            for pid in $pids
                kill -15 $pid
                echo SIGTERM pid $pid and port $port
            end
            sleep 0.5
            for pid in (lsof -ti :$port)
                kill -9 $pid
                echo SIGKILL pid $pid and port $port
            end
        end
    else
        echo "Usage: killport <port>"
    end
end
