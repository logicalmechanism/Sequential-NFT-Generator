import Prelude
import Cardano.Api
import StatelessMintingContract ( mintingPlutusScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "stateless-minting-contract.plutus" Nothing mintingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()