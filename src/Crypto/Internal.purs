module Crypto.Internal where

import Data.Function.Uncurried as Fn
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Uncurried as EU

type Keypair =
  { private :: PrivateKey
  , public :: PublicKey
  }

newtype PrivateKey = PrivateKey String
derive instance newtypePrivateKey :: Newtype PrivateKey _

newtype PublicKey = PublicKey String
derive instance newtypePublicKey :: Newtype PublicKey _

generateRSAKeypair :: Effect Keypair
generateRSAKeypair = EU.runEffectFn2 _generateRSAKeypair PrivateKey PublicKey

makePrivateKey :: String -> PrivateKey
makePrivateKey = PrivateKey

makePublicKey :: String -> PublicKey
makePublicKey = PublicKey

rsaVerify :: PublicKey -> String -> String -> Boolean
rsaVerify key signature x = Fn.runFn3 _rsaVerify key signature x

unPrivateKey :: PrivateKey -> String
unPrivateKey = unwrap

unPublicKey :: PublicKey -> String
unPublicKey = unwrap

foreign import _generateRSAKeypair :: EU.EffectFn2 (String -> PrivateKey) (String -> PublicKey) Keypair
foreign import _rsaVerify :: Fn.Fn3 PublicKey String String Boolean
