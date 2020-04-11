{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude

-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query
  = ByTag Text
  deriving (Eq, Show)

data MatchContent = MatchContent
  { matchTextualID :: Text
  }

instance ToJSON MatchContent where
  toJSON MatchContent {..} =
    object
      [ "textualID" .= matchTextualID
      ]

matchQuery :: Zettel c -> Query -> Bool
matchQuery Zettel {..} = \case
  ByTag tag -> tag `elem` zettelTags

extractMatchContent :: Zettel c -> Zettel MatchContent
extractMatchContent = extendContent $ \Zettel {..} ->
  MatchContent
    { matchTextualID = zettelIDText zettelID
    }

runQuery :: ZettelStore -> [Query] -> [Zettel MatchContent]
runQuery store queries =
  fmap extractMatchContent $ flip filter (Map.elems store) $ \z ->
    and $ matchQuery z <$> queries
