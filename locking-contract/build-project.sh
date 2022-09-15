cabal build -w ghc-8.10.7
cabal run locking-contract

cardano-cli address build --payment-script-file locking-contract.plutus --testnet-magic 1097911063 --out-file validator.addr
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\nValidator Testnet Address:" $(cat validator.addr)
echo -e "\nValidator Hash:" $(cat validator.hash)
echo -e "\nValidator Bytes:" $(cat validator.bytes)
echo "DONE"