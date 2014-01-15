------------------------------------------------------------------------------

-- | Holds D3 level primitives.
--   TODO break me up as necessary!

------------------------------------------------------------------------------

module Soostone.Graphing.Chart.Element where

import Soostone.Graphing.D3
import Soostone.Graphing.Chart.Base
import Soostone.Graphing.Theme

------------------------------------------------------------------------------

-- | Wraps a Graph in an SVG group.

group :: Chart a () -> Chart a ()
group = append "g"

-- | Wraps a Graph in an SVG rect.

rect :: Chart a () -> Chart a ()
rect inner = append "rect" $ do
    defaultSize
    inner

------------------------------------------------------------------------------
