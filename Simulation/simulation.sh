# Unset P2PRC
unset P2PRC

# Create P2PRC instances
# Parameters:
# 1. Number of nodes
P2PRC_instances() {
  local rootPort=""
  for (( counter=0; counter<$1; counter++ )); do
    mkdir  test-$counter/
    cd test-$counter/

    # Generate a new random port
    NEW_PORT=$(shuf -i 2000-65000 -n 1)

    # Initialize node
    p2prc --dc

    # Save rootPort from the first node
    if [[ $counter -eq 0 ]]; then
      rootPort=$NEW_PORT
    fi

    echo "Root port: $rootPort"
    echo "This node's port: $NEW_PORT"

    # Set root port
    p2prc --arn --ip 0.0.0.0 -p "$rootPort"

    # add current node
    p2prc --as 0.0.0.0 -p "$NEW_PORT"

    # Replace the ServerPort in config
    sed -i.bak "s/\"ServerPort\": \"[0-9]*\"/\"ServerPort\": \"$NEW_PORT\"/" "./config.json"
    sed -i.bak "s/\"Test\": false/\"Test\": true/" "./config.json"
    sed -i.bak "s/\"BehindNAT\": true/\"BehindNAT\": false/" "./config.json"

    cat config.json

    # List services
    p2prc --ls

    cd ..
  done

  # remove all instances after testing (optional — might want to comment this during testing)
  # rm -rf test-*
}

# Arrays of process IDs
pids=()

# Calls all the P2PRC instances created for testing
Start_all_instances() {
    for (( counter=0; counter<$1; counter++ )); do
        cd test-$counter/
        # Start P2PRC as background process
        p2prc -s &
        pids+=($!)
        sleep 3
        cd ..
    done
}

# Kills all P2PRC instances
Kill_all_instances() {
  for pid in "${pids[@]}"; do
    kill "$pid"
    echo "Killed process $pid"
  done
}

# Removes all test files created
Remove_all_test_files() {
    rm -rf test-*
}

# List ip tables of all nodes started
IP_Tables_after_Started() {
  for (( counter=0; counter<$1; counter++ )); do
    cd test-$counter/
    p2prc --ls
    cd ..
  done
}

Update_All_IP_Tables() {
  for (( counter=0; counter<$1; counter++ )); do
    cd test-$counter/
    p2prc --us
    cd ..
  done
}


# ---------------- Work flow test ---------------

# Unset default P2PRC env path
unset P2PRC

# Run with 2 nodes
P2PRC_instances 3

## Start instances
Start_all_instances 3

## List ip tables of nodes started
IP_Tables_after_Started 3
#
## Remove test files created
Remove_all_test_files

## Kill all instances
Kill_all_instances
