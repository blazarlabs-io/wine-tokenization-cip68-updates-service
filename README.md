![Banner](out/wine.jpg)

# Wine Supply Chain Tracking and Reporting System 🍷🍇

- [Wine Supply Chain Tracking and Reporting System 🍷🍇](#wine-supply-chain-tracking-and-reporting-system-)
  - [1. Overview](#1-overview)
  - [2. Architecture \& Components](#2-architecture--components)
  - [3. Project Structure](#3-project-structure)
  - [4. Installation \& Setup](#4-installation--setup)
    - [4.1 Development setup](#41-development-setup)
  - [5. Operation](#5-operation)
    - [5.1.  Configure admin key](#51--configure-admin-key)
    - [5.2.  **Configure `atlas_config.json`**](#52--configure-atlas_configjson)
    - [5.3 Docker setup](#53-docker-setup)
      - [5.3.1a Build docker image](#531a-build-docker-image)
      - [5.3.1b Pull docker image](#531b-pull-docker-image)
      - [5.3.2 Run a new containter](#532-run-a-new-containter)
      - [5.3.3 Add the configuration files](#533-add-the-configuration-files)
      - [5.3.4 Restart](#534-restart)
  - [6. Usage](#6-usage)
    - [6.1. Pinata API](#61-pinata-api)
    - [6.2. Manual -  Deploy validators](#62-manual----deploy-validators)
    - [6.3. Manual Start the service](#63-manual-start-the-service)
  - [7. License](#7-license)
  - [8. Contributions, Feedback and Support](#8-contributions-feedback-and-support)
  - [9. Future Milestones](#9-future-milestones)
  - [10. Acknowledgments](#10-acknowledgments)

---

## 1. Overview

This is the backend and smart contract service that supports the system for wineries and consumers to produce the wine token and track the quality of wine.
This project interacts with both Cardano-Node ( L1 ) and Hydra-Head ( L2 ).
Using the CIP-68 standard, wine collections (batches) and individual bottles are represented as NFTs on the Cardano blockchain.

- A wine collection (batch) is represented on-chain via a set of descriptive attributes and a set of measurements unique to that collection.
- An individual bottle is represented on-chain via a reference to that collection, a set of descriptive attributes unique to that bottle, a set of measurements, and a set of inscriptions representing data/notes unique to that bottle.

The values ​​of the descriptive attributes and measurements are stored in the Pinata Cloud, and the file structure includes the current data and the hash of the previous file.

---

## 2. Architecture & Components

![ComponentDiagram](out/component_diagram/ComponentDiagram.png)

> -  **Web Tokensation Service**  Service for management of tokenized wine collections and individual bottles.
> -  **IPFS Node**: Local IPFS Node or IPFS Provider.
> -  **Cardano Node**: Local Cardano node or Cardano Blockchain Provider
---

## 3. Project Structure

```plaintext
.
├── src
|	├──  Admin                          # Administrator cli interface (onchain deployment of validators)
|	├──  Bruno                          # Bruno collection for the REST API.
|	├──  Offchain                       # Offchain logic for transaction building
|	├──  Onchain                        # Cardano smart contracts (spending validators and minting policies)
|	├─── Server                         # Server exposing REST API interface for wine tokens
|	└──  Tests                          # Testing of operations (transactions).
|
├── wine_contract_blueprint.json   		# Blueprint documenting the smart contract 
├── wine_validator.plutus          		# Serialized validator
├── config_atlas.json              		# Atlas configuration file for the blockchain provider
├── payment.key                    		# Admin key
├── config_admin.json              		# Administrator context config file (contains reference script UTxO and admin key)
├── swagger-api             			# Wine API Swagger file
└── README.md                      		# This file

```

## 4. Installation & Setup


### 4.1 Development setup

This project uses the [The Developer Experience Shell](https://github.com/input-output-hk/devx/#the-developer-experience-shell) to build a fully-functioning and reproducible Cardano development shell for Haskell quickly and across multiple operating systems (and architectures).


 * After installing and configuring `nix` and `direnv`, clone the repo and type:

 ```
 direnv allow
``` 

 * The test suite for operations (transactions) can be run with the following command:

```
cabal test 
```

## 5. Operation


***
### 5.1.  Configure admin key
***
  * This service is managed by an owner who pays for the submited transactions fees.  For this purpose, a file named **"payment.skey"**, which should have the following format must exist.

```
{
    "type": "PaymentSigningKeyShelley_ed25519",
    "description": "Payment Signing Key",
    "cborHex": "5820f56ce81846c1d67c939fea0e2c6e8d9693fb5c922ee567c6365561ada1bd45e1"
}

```
 * A new key could be generated using cardano-cli:
```
cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey
```

 * The next step would be to generate a wallet address for the payment key and make sure that funds are available  For tesnets you can use the [Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet).
```
cardano-cli address build \
    --payment-verification-key-file payment.vkey \
    --out-file payment.addr \
```


***
### 5.2.  **Configure `atlas_config.json`**
***

  * Building transaction bodies requires gathering suitable information from the blockchain.  For this purpose, we'll require a provider. So at the project root directory a file named **"config_atlas.json"**, which should have the following format

```
{
  "coreProvider": {
    "maestroToken": "{YOUR TOKEN HERE}"
  },
  "networkId": "testnet-preview",
  "logging": [
    {
      "type": {
        "tag": "stderr"
      },
      "severity": "Debug",
      "verbosity": "V2"
    }
  ],
  "utxoCacheEnable": true
}
```
 * More info about the provider config can be found [here](https://atlas-app.io/getting-started/endpoints#defining-provider-configuration)



### 5.3 Docker setup

#### 5.3.1a Build docker image

Optionally you can build the image yourself:

```
docker buildx build \
  --platform linux/amd64 \
  -t your-registry/your-image:latest \
  .
```

#### 5.3.1b Pull docker image

You can pull the docker image for amd64 from here.

```
docker pull ghost0126/wine-tokenization-service:v2
```


* A container of this image will start the ipfs daemon and the wine service (with default test configuration) listening on http://0.0.0.0:8082/  (swagger on http://0.0.0.0:8082/swagger-ui)


#### 5.3.2 Run a new containter

It is recommended that you use a volume for ipfs storage to ensure you do not lose data.
Eg. of running the server (with default test configuration) and 'youruser' 'yourpassword' for the Basic Auth:

- 8082 (is the wine server port)


```
docker run -d \
  -e PINATA_API_KEY=<your pinata api key> \
  -e PINATA_SECRET_API_KEY=<your pinata secret api key> \
  -p 8082:8082 \
  --name <name> \
  ghost0126/wine-tokenization-service:v2 server <username> <userpassword>
```

#### 5.3.3 Add the configuration files

```
docker cp payment.skey <name>:/wine/
docker cp config_atlas.json <name>:/wine/
```

#### 5.3.4 Restart 

```
docker restart <name>
```


## 6. Usage

### 6.1. Pinata API

Let's assume you have a local image called yourimage.png. You can upload it to Pinata Cloud via a direct HTTP request to the API.

```
curl -u <youruser>:<yourpassword> -X POST http://localhost:8082/add \
     -H "Content-Type: application/octet-stream" \
     --data-binary @yourimage.png
```

The response will be something like:

```
ipfs://QmdkrDhHFKd2TEvwTi6oxWc91Q9xojJVpc6UQMMg29ez7g
```
Remember Hash to be able to access the image via IPFS.
View image via gateway (port 8080).
Open address in browser:
```
https://gateway.pinata.cloud/ipfs/QmdkrDhHFKd2TEvwTi6oxWc91Q9xojJVpc6UQMMg29ez7g
```

***
### 6.2. Manual -  Deploy validators
***

 * This service requires validators to be deployed and to be used as reference UTxOs in the transactions.  The validators can be deployed with the following command.

```
admin deploy-validator payment.skey
```

* This will output :
	- a file named **"config_admin.json"** which contains the validators reference UTxO and the admin key.
	- a file named **"wine_validator.plutus"** which contains the serialized validator.
	- a file named **"wine_contract_blueprint"** which contains the validator's blueprint.

***
### 6.3. Manual Start the service
***

 * Once the validators are deployed, the service can be started with the following command:

```
server [-- <user> <pass>]
```
 * This command will start the server listening on http://0.0.0.0:8082/ and serve the swagger on http://0.0.0.0:8082/swagger-ui
 * Optionally if *user* and *pass* arguments are passed, they will be used for the *Basic Authentication* instead of the default values.


## 7. License

See the [LICENSE](LICENSE.md) file for details.


## 8. Contributions, Feedback and Support

We welcome contributions from the community! Your feedback is invaluable!
Use the following channels for support or feedback:

1. Report bugs or suggest features via [GitHub Issues] .
2. Join the Conversation: [GitHub Discussions] .
3. Submit pull requests (PRs) that align with the project's goals.

## 9. Future Milestones

[Milestones](https://milestones.projectcatalyst.io/projects/1200246)


## 10. Acknowledgments
Thanks to the Cardano community for support.
This project is funded by [F12 - Project Catalyst ID: #1200246 ](https://projectcatalyst.io/funds/12/cardano-use-cases-mvp/wine-supply-chain-tracking-and-reporting-system)

---
