# Hydra Integration Guide

## Prerequisites
- Cardano node running
- Hydra node running on port 8082
- IPFS node running
- Required Haskell dependencies

## Setup Steps

### 1. Environment Configuration
```bash
# Set up environment variables
export HYDRA_NODE_URL="http://localhost:8082"
export IPFS_API_URL="http://localhost:5001"
export CARDANO_NODE_URL="http://localhost:8090"
```

### 2. API Endpoints

#### Token Migration
```http
POST /migrate
Content-Type: application/json

{
    "migrationTokenId": "token_id",
    "migrationAmount": 1
}
```

#### Commit to Hydra
```http
POST /commit
Content-Type: application/json

{
    "commitTokenId": "token_id",
    "commitAmount": 1,
    "commitMetadata": "optional_metadata"
}
```

#### Decommit from Hydra
```http
POST /decommit
Content-Type: application/json

{
    "decommitTokenId": "token_id",
    "decommitAmount": 1
}
```

### 3. Integration Steps

1. **Initialize Hydra Head**
   - Call `initHead` with participant addresses
   - Wait for head to be initialized

2. **Migrate Tokens**
   - Use `/migrate` endpoint to move tokens from L1 to Hydra
   - Verify transaction status

3. **Perform Operations**
   - Use commit/decommit endpoints for token operations
   - Monitor Hydra head state

4. **Error Handling**
   - Implement proper error handling for all operations
   - Monitor transaction status

### 4. Testing

1. **Unit Tests**
```haskell
testMigration :: IO ()
testMigration = do
    ctx <- createTestContext
    resp <- migrateToHydraWorkflow ctx testTokenId 1
    assertSuccess resp
```

2. **Integration Tests**
```haskell
testHydraWorkflow :: IO ()
testHydraWorkflow = do
    ctx <- createTestContext
    -- Test complete workflow
    resp <- commitWorkflow ctx testCommitRequest
    assertSuccess resp
```

### 5. Monitoring

1. **Health Checks**
   - Monitor Hydra node status
   - Check L1 connection
   - Verify IPFS connectivity

2. **Logging**
   - Implement proper logging
   - Monitor transaction status
   - Track error rates

## Troubleshooting

### Common Issues
1. Hydra node connection issues
2. Transaction failures
3. State synchronization problems

### Solutions
1. Verify node connectivity
2. Check transaction parameters
3. Monitor head state

## Security Considerations

1. **Key Management**
   - Secure key storage
   - Proper access control
   - Regular key rotation

2. **Transaction Security**
   - Validate all inputs
   - Implement proper error handling
   - Monitor for suspicious activity

## Support

For additional support:
- Check documentation
- Contact development team
- Monitor system logs 