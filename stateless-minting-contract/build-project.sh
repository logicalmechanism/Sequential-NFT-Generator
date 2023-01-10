
rm stateless-minting-contract.plutus
rm policy.id
rm policy.bytes

cabal build -w ghc-8.10.7
cabal run stateless-minting-contract

cardano-cli transaction policyid --script-file stateless-minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes


# nft minting hash
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"