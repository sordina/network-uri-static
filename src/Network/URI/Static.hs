{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Network.URI.Static
    ( staticURI
    , uri
    , auri
    , ruri
    ) where

import Language.Haskell.TH (unType)
import Language.Haskell.TH.Lib (TExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Network.URI as U
import Data.Char (isSpace)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes

type Parser =  String -> Maybe U.URI

-- | 'staticURI' parses a specified string at compile time
--   and return an expression representing the URI when it's a valid URI.
--   Otherwise, it emits an error.
--
-- >>> $$(staticURI "http://www.google.com/")
-- http://www.google.com/
--
-- >>> $$(staticURI "/drums/")
-- /drums/
--
-- >>> $$(staticURI "http://www.google.com/##")
-- <BLANKLINE>
-- <interactive>...
-- ... Invalid URI: http://www.google.com/##
-- ...
staticURI :: Parser
          -> String -- ^ String representation of a URI
          -> TExpQ U.URI -- ^ URI
staticURI parser (parser -> Just u) = [|| u ||]
staticURI _ u = fail $ "Invalid URI: " ++ u

instance Lift U.URI where
    lift (U.URI {..}) = [| U.URI {..} |]

instance Lift U.URIAuth where
    lift (U.URIAuth {..}) = [| U.URIAuth {..} |]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | 'uri' is a quasi quoter for 'staticURI'.
--
-- >>> [uri|http://www.google.com/|]
-- http://www.google.com/
--
-- >>> [uri| /guitars/ |]
-- /guitars/
--
-- >>> [uri|http://www.google.com/##|]
-- <interactive>:53:6: error:
--  • Invalid URI: http://www.google.com/##
--  • In the quasi-quotation: [uri|http://www.google.com/##|]
uri :: QuasiQuoter
uri = QuasiQuoter {
    quoteExp  = fmap unType . staticURI U.parseURIReference . trim,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = undefined
}

auri :: QuasiQuoter
auri = QuasiQuoter {
    quoteExp  = fmap unType . staticURI U.parseURI . trim,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = undefined
}

ruri :: QuasiQuoter
ruri = QuasiQuoter {
    quoteExp  = fmap unType . staticURI U.parseRelativeReference . trim,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = undefined
}

