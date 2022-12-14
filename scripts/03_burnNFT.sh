#!/bin/bash
set -e
if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Token Number'
    exit 1
fi
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

script_path="../locking-contract/locking-contract.plutus"
mint_path="../minting-contract/minting-contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
collat_address=$(cat wallets/collat-wallet/payment.addr)
#
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)
#
deleg_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/delegator-wallet/payment.vkey)
#
policy_id=$(cat ../minting-contract/policy.id)
#

token_name=$(cat ../start_info.json | jq -r .starterTkn)

#pass in the token number to this script
name=${token_name}$(echo -n "${1}" | xxd -ps)

MINT_ASSET="-1 ${policy_id}.${name}"
#
start_id=$(cat policy/starter.id)
token_name=$(cat ../start_info.json | jq -r .starterTkn)
START_ASSET="1 ${start_id}.${token_name}"

script_address_out="${script_address} + 5000000 + ${START_ASSET}"

starter_nft_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/worst_case_datum.json | tr -dc '0-9')
    
echo "Starter NFT Min Fee: "${starter_nft_min_utxo}

script_address_out="${script_address} + ${starter_nft_min_utxo} + ${START_ASSET}"
echo "Script OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${buyer_address} \
    --out-file tmp/buyer_utxo.json

TXNS=$(jq length tmp/buyer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
   exit;
fi
alltxin=""
# Gather only UTxO that are ada-only or are holding the correct NFT
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$policy_id" --arg name "$name" 'to_entries[] | select((.value.value | length < 2) or .value.value[$policy_id][$name] == 1) | .key | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
buyer_tx_in=${TXIN::-8}

# echo "buyer_tx_in: $buyer_tx_in"

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json

# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}
# echo "script_tx_in: $script_tx_in"

echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json

TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
reference_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/reference-wallet/payment.vkey)
delegator_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/delegator-wallet/payment.vkey)

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${buyer_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/burn_redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/current_datum.json \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${seller_pkh} \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file data/mint_redeemer.json \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/buyer-wallet/payment.skey \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed
