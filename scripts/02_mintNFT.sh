#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

#
script_path="../locking-contract/locking-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
#
mint_path="../minting-contract/minting-contract.plutus"
#
ft_script_path="../locking-contract/locking-contract.plutus"
ft_script_address=$(${cli} address build --payment-script-file ${ft_script_path} --testnet-magic ${testnet_magic})
#
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
# buyer_address="addr_test1qrupt9d9ug2ufnrrajp2q7gwvmrtzzgr80p5ug7q8nt4d66hu0s5mnhxh2853wtsgn9gdz6wuqtaqnkv0yk78p474d6qudapqh"
#
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
#
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
#

#
policy_id=$(cat ../minting-contract/policy.id)
#
token_name=$(cat ../start_info.json | jq -r .starterTkn)
token_number=$(cat data/current_datum.json | jq -r .fields[1].int)

name=${token_name}$(echo -n "${token_number}" | xxd -ps)
#
MINT_ASSET="1 ${policy_id}.${name}"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${buyer_address} + 5000000 + ${MINT_ASSET}" | tr -dc '0-9')
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
buyer_address_out="${buyer_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
echo "Token Script OUTPUT: "${script_address_out}
echo "Mint OUTPUT: "${buyer_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

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

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${seller_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/mint_redeemer.json \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/next_datum.json \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
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


t=$(cat data/next_datum.json | jq .fields[1].int)
ft=$((t+1))
variable=${ft}; jq -r --argjson variable $variable '.fields[1].int=$variable' data/next_datum.json > data/next_datum-new.json
mv data/next_datum-new.json data/next_datum.json


t=$(cat data/current_datum.json | jq .fields[1].int)
ft=$((t+1))
variable=${ft}; jq -r --argjson variable $variable '.fields[1].int=$variable' data/current_datum.json > data/current_datum-new.json
mv data/current_datum-new.json data/current_datum.json