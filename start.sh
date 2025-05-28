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

# Set Pinata environment variables
export PINATA_API_KEY="c84bd99f13c11e6f846d"
export PINATA_SECRET_API_KEY="2b25c28b7811f8d00af49780497f93b706597e3a5fedd434e834e7c2bba3082b"

# Start the wine tokenization service
echo "Starting wine tokenization service..."
server "$@"

# Keep the container running
wait $IPFS_PID 