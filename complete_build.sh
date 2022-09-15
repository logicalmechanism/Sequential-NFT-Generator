#!/bin/bash
# set -e
if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Token Name That Will Be Used For The Starter  And The Catalog Name'
    exit 1
fi
# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

minter_pkh=$(cat start_info.json | jq -r .minter)

# set up start info
variable=${minter_pkh}; jq --arg variable "$variable" '.scripts[0].keyHash=$variable' scripts/policy/policy.script > scripts/policy/policy-new.script
mv scripts/policy/policy-new.script scripts/policy/policy.script


export CARDANO_NODE_SOCKET_PATH=$(cat scripts/path_to_socket.sh)
cli=$(cat scripts/path_to_cli.sh)
testnet_magic=$(cat testnet.magic)
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq -r .slot)
deltaT=$(cat start_info.json | jq -r .cutOff)
cutOff=$((${slot} + ${deltaT}))

variable=${cutOff}; jq -r --argjson variable $variable '.scripts[1].slot=$variable' scripts/policy/policy.script > scripts/policy/policy-new.script
mv scripts/policy/policy-new.script scripts/policy/policy.script

# exit

cardano-cli transaction policyid --script-file scripts/policy/policy.script > scripts/policy/starter.id
policy_id=$(cat scripts/policy/starter.id)

tkn_name=${1}
tkn_name=$(echo ${tkn_name:0:32})
echo -e "\033[1;36m Token Name: ${tkn_name} \033[0m"

token_name=$(echo -n ${tkn_name} | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
echo -e "\033[1;36m Starter Token ${policy_id}.${token_name} \033[0m"

variable=${policy_id}; jq --arg variable "$variable" '.starterPid=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=${token_name}; jq --arg variable "$variable" '.starterTkn=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json

variable=${token_name}; jq --arg variable "$variable" '.fields[2].bytes=$variable' scripts/data/current_datum.json > scripts/data/current_datum-new.json
mv scripts/data/current_datum-new.json scripts/data/current_datum.json
variable=${token_name}; jq --arg variable "$variable" '.fields[2].bytes=$variable' scripts/data/next_datum.json > scripts/data/next_datum-new.json
mv scripts/data/next_datum-new.json scripts/data/next_datum.json

# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .starterPid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .starterTkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn
python3 -c "import binascii;a=$(cat start_info.json | jq .minter);s=binascii.unhexlify(a);print([x for x in s])" > minter.pkh


# Adds the delegator to the nft locking and minting contracts
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract.hs-new.hs', $(cat minter.pkh))"
mv ./locking-contract/src/LockingContract.hs-new.hs ./locking-contract/src/LockingContract.hs
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract.hs-new.hs', $(cat minter.pkh))"
mv ./minting-contract/src/MintingContract.hs-new.hs ./minting-contract/src/MintingContract.hs

# Adds in the locking token into the contract.
python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract-new.hs', $(cat start.pid))"
mv ./locking-contract/src/LockingContract-new.hs ./locking-contract/src/LockingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract-new.hs', $(cat start.tkn))"
mv ./locking-contract/src/LockingContract-new.hs ./locking-contract/src/LockingContract.hs

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat start.pid))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat start.tkn))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs


# build
cd locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run locking-contract
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
# locking validator hash
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"


cd ..


# adds in the locking hash into the script
python3 -c "from update_contracts import changeLockHash;changeLockHash('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat ./locking-contract/validator.bytes))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs


# build nft minting
cd minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run minting-contract
cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

# nft minting validator hash
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"


# update the datums for the nft generator
cd ../scripts/data
variable=$(cat ../../minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' current_datum.json > current_datum-new.json
mv current_datum-new.json current_datum.json
variable=$(cat ../../minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' next_datum.json > next_datum-new.json
mv next_datum-new.json next_datum.json

# reset datum
ft=0
variable=${ft}; jq --argjson variable $variable '.fields[1].int=$variable' current_datum.json > current_datum-new.json
mv current_datum-new.json current_datum.json
ft=1
variable=${ft}; jq -r --argjson variable $variable '.fields[1].int=$variable' next_datum.json > next_datum-new.json
mv next_datum-new.json next_datum.json

cd ../..

find . -name '*.hash' -type f -exec sha256sum {} \; > hash.hashes
echo -e "\033[1;36m \nvalidator sha256sum \033[0m"
echo -e "\033[1;33m $(cat hash.hashes) \033[0m"

find . -name 'policy.id' -type f -exec sha256sum {} \; > policy.hashes
echo -e "\033[1;36m policy sha256sum \033[0m"
echo -e "\033[1;33m $(cat policy.hashes) \033[0m"

find . -name '*.hashes' -type f -exec sha256sum {} \; > final.check
echo -e "\033[1;35m \nfinal sha256sum \033[0m"
echo -e "\033[1;32m $(sha256sum final.check) \033[0m"

#
# exit
#