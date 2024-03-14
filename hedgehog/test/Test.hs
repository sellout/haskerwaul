import Data.Functor (fmap)
import Hedgehog
import qualified Hedgehog.Internal.Property as Hedgehog
import Hedgehog.Main
import System.IO
import Test.Set
import Prelude (concat, ($), (.))

main :: IO ()
main =
  defaultMain . fmap (checkParallel . freelyDiscard) $
    concat
      -- Add category groups here.
      [ setProperties
      ]

-- | The only way (I think) to have generators for initial objects (e.g.,
--  `Data.Void.Void`) is to have them `Gen.discard`. Since we do a lot of stuff
--   with binary tensors and units, this means we'll fail a lot of properties
--   with the default discard limit. So we bump it here for all of our
--   properties.
--
--   Very open to alternative approaches!
freelyDiscard :: Hedgehog.Group -> Hedgehog.Group
freelyDiscard group =
  group
    { groupProperties =
        fmap
          ( fmap
              ( \prop ->
                  prop
                    { Hedgehog.propertyConfig =
                        (Hedgehog.propertyConfig prop) {Hedgehog.propertyDiscardLimit = 1000}
                    }
              )
          )
          $ groupProperties group
    }
