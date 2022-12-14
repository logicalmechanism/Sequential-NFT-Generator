#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)
#
#
ft_script_path="../locking-contract/locking-contract.plutus"
FT_SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${ft_script_path} --testnet-magic ${testnet_magic})


#
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat wallets/reference-wallet/payment.addr)
COLLAT_ADDRESS=$(cat wallets/collat-wallet/payment.addr)

#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo
echo -e "\033[1;35m Lock Script Address:" 
echo -e "\n${FT_SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${FT_SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;36m Minter Address:" 
echo -e "\n${SELLER_ADDRESS}\n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Buyer Address:" 
echo -e "\n${BUYER_ADDRESS}\n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS}\n";
${cli} query utxo --address ${REFERENCE_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${COLLAT_ADDRESS}\n";
${cli} query utxo --address ${COLLAT_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

