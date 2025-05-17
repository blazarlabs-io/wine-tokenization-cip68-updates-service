#!/bin/bash

# Initialize IPFS if not already initialized
if [ ! -d "/root/.ipfs" ]; then
    echo "Initializing IPFS..."
    ipfs init
fi

# Configure IPFS
ipfs config Addresses.Gateway /ip4/0.0.0.0/tcp/8080
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin "[\"*\"]"
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Methods "[\"PUT\", \"GET\", \"POST\"]"
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Credentials "[\"true\"]"

# Start IPFS daemon in the background
echo "Starting IPFS daemon..."
ipfs daemon &
IPFS_PID=$!

# Wait for IPFS to be ready
echo "Waiting for IPFS to be ready..."
sleep 5

# Start the wine tokenization service
echo "Starting wine tokenization service..."
server "$@"

# Keep the container running
wait $IPFS_PID 