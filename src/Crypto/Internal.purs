module Crypto.Internal where

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)

type Keypair = 
  { private :: PrivateKey
  , public :: PublicKey
  }

newtype PrivateKey = PrivateKey String
derive instance newtypePrivateKey :: Newtype PrivateKey _

newtype PublicKey = PublicKey String
derive instance newtypePublicKey :: Newtype PublicKey _

generateRSAKeypair :: Effect Keypair
generateRSAKeypair = _generateRSAKeypair PrivateKey PublicKey

unPrivateKey :: PrivateKey -> String
unPrivateKey = unwrap

unPublicKey :: PublicKey -> String
unPublicKey = unwrap

foreign import _generateRSAKeypair :: (String -> PrivateKey) -> (String -> PublicKey) -> Effect Keypair
