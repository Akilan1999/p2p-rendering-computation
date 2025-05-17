# Spin up seperate P2PRC instances

# Unset P2PRC
unset P2PRC


# Create P2PRC instances
# Parameters:
# 1. Number of nodes
P2PRC_instances() {
  local rootPort
  for (( counter=0; counter<$1; counter++ )); do
    mkdir test-$counter/
    cd test-$counter/
    p2prc --dc

    # change port no
    NEW_PORT=$(shuf -i 2000-65000 -n 1)

    if [ counter == 0 ]; then
        rootPort = $NEW_PORT
    fi

    echo $rootPort

    p2prc --arn --ip 0.0.0.0 --port $rootPort

    sed -i.bak "s/\"ServerPort\": \"[0-9]*\"/\"ServerPort\": \"$NEW_PORT\"/" "./config.json"

    p2prc --ls

    cd ..
   done

   # remove all instances
   rm -rf test-*
}

P2PRC_instances 2


