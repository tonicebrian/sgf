module Data.SGF (
    module Data.SGF.Types,
    module Data.SGF.Parse
) where

-- It should be noted that the SGF specification talks not only about the file
-- format, but also about the behavior of applications that read and write SGF
-- files.  This library cannot guarantee that applications conform to that
-- specification.

import Data.SGF.Types
import Data.SGF.Parse (collection)

-- TODO:
-- * support the rest of the SGF format ;-)
-- * clean up exports and imports as much as possible
-- * documentation for usage
-- * commenting/documentation for hacking
