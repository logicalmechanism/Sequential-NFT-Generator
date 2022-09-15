# A Sequential NFT Generator

This is a 3 / 8 fork of the tokenization and fractionalization contracts from NEWM, https://github.com/projectNEWM/contracts. It has been strip down into a single signer, single collection, sequential nft generator. This contract will allow a user to mint a very large amount of nfts that are sequential in their name. This means that a name is defined with a prefix like meta_land_ or starter_token_ then it is joined together with the current counter, forming tokens like meta_land_415 and starter_token_0. 

This contract is very simple but also incredibly useful. A lot of dApps require an NFT to start the contract off. Generating these NFTs can lead centralization and trust issues. These issues can be alleviated by having a contract that gaurantees the user an NFT every time they use it. This is great for projects that fear of the double utxo attack or insider double mint attacks. These fears are now a thing of the past. Projects can now safetly generate actually starter NFTs for their project using this contract.

## How It Works

The NFT generator comes as a pair of smart contracts. The pair forms something called a lock and mint. The current state of the mint is held in the datum of a locked utxo. When an NFT is minted, the datum is updated to reflect the new mint. The contracts are designed to only work together and they do not function in singular form. This means a locked utxo may not be updated unless a mint occurs and a mint can only occur if the locked utxo is updated. This paired security makes this contract very safe to use.

```hs
data CustomDatumType = CustomDatumType
  { cdtNewmPid :: PlutusV2.CurrencySymbol
  -- ^ The policy id from the minting script.
  , cdtNumber  :: Integer
  -- ^ The starting number for the collection.
  , cdtPrefix  :: PlutusV2.BuiltinByteString
  -- ^ The prefix for a collection.
  }
```

The counter, cdtNumber, typically starts are zero but it may start at any integer the user may want. A key note about the prefix is that it will implicitly determine the maximum amount of NFTs that can be minted. Since the maximum token name length is 32 characeters then prefixes like , some_long_prefix_for_a_token_, will only allow for 1000 NFTs, some_long_prefix_for_a_token_0 to some_long_prefix_for_a_token_999. For short prefixes this will not be an issue.

## Test Script Flow

There is a complete build script as well as test scripts that allow a set of wallets to interact with the smart contract via the CLI. The complete_build.sh script takes in a prefix name then it will auto-build all the contracts, create the starter nft, and update the datums. That scripts uses data inside the start_info.json file.

```json
{
  "__comment1__": "This is the primary pkh for minting and burning.",
  "minter": "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439",
  "__comment2__": "The length of time the starter nft policy script is open.",
  "cutOff": 500,
  "__comment3__": "These two fields are auto filled with complete_build.sh",
  "starterPid": "3c9a5265ff606e57550319cd025a6c062b9cf4349fbf9f913d45a3ea",
  "starterTkn": "737461727465725f746f6b656e5f"
}
```

The start json holds the minter pkh and the cut off for the starter NFT policy. If the build and network speed is slow then a larger cutOff may be required to ensure that the transactions does hit the chain in time before the starter policy is locked. The last starter token fields are auto-filled with the complete build script. After the build scripts complets, it will output a hash of the build that can be used to cross-checking builds.

The test scripts are found in the scripts folder. They are numbered for the order. The scripts require a live and fully sync testnet node. Change the testnet.magic file contents to match which ever network you would want to point too. The scripts do assume some wallets that are not included in this repo. Please create the wallets with the cli and use the faucet to receive test funds. There is a seller, buyer, collat, and reference wallet. Enterprise or stake wallets may be used.

The zeroth step is creating script references. This is essential. The seller will send ADA to the reference wallet, two utxos that hold the lock and mint scripts. The first step is minting the starter NFT and prepping with the lock contract with it in one transaction. The policy script is designed to lock in a short time frame to help ensure that the starter token is truly an NFT. The third step is minting an NFT and sending it to some address. The script will auto increment the datum files for a nice automated process. The fourth step is burning some NFT. This requires sending the locked utxo back to the lock contract with the same datum. This will allow any NFT on the policy to be burned.

## Example


```bash
./complete_build.sh starter_token_

# validator sha256sum 
# 287d793d5ce7b4a6448383509b7fe34ba92065ac3386bb472e7d08079653ea98  ./minting-contract/validator.hash
# 33132a11a522443a95f19bd3c58a4426ae841117e161454b6d292849e660aab3  ./locking-contract/validator.hash 

# policy sha256sum 
# 1d8b2b40fbc80e853ef004d9d5be5ccb653b1009d511875855d8b3e6201a91f7  ./minting-contract/policy.id 

# final sha256sum 
# 7063137fd01bbdefc1be17d3962ba2adc3a9635608c9604323e4d08ef93a8a30  final.check 
```

```bash
./00_createReferenceScript.sh
# FT Locking Min Fee 24321330
# FT Minting Min Fee 17084840

./01_mintStarterNft.sh
# Mint OUTPUT: addr_test1wphvv7apftn2eghm20lta0tryqjne8azsyarnjvfvyrky4qm0nxdn + 1439540 + 1 ab4efb7d1324421b1adf8468df73bb3fb6b1c8001d19b24b7975236c.737461727465725f746f6b656e5f

./02_mintNFT.sh
# Mint OUTPUT: addr_test1qrupt9d9ug2ufnrrajp2q7gwvmrtzzgr80p5ug7q8nt4d66hu0s5mnhxh2853wtsgn9gdz6wuqtaqnkv0yk78p474d6qudapqh + 1193870 + 1 93e8f8d9e635f912483c49c4576929bd31d359e749be32357d5b1338.737461727465725f746f6b656e5f30

./03_burnNFT.sh 0
```