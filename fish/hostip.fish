# Get current host ip
function hostip
    set ip (ip route get 1 | grep -E -o 'src[[:space:]][0-9]+[.][0-9]+[.][0-9]+[.][0-9]+' | awk '{print $2}')
    # Explicitly export as enviroment variable
    set -gx HOST_IP $ip
    echo $ip
end
