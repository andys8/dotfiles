function killport
    if test -n "$argv"
        set port $argv[1]
        set pid (lsof -ti :$port)
        if test -n "$pid"
            kill $pid
            echo Killed process with pid $pid and port $port
        else
            echo No process with port $port
        end
    else
        echo "Usage: killport <port>"
    end
end
