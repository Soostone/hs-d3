------------------------------------------------------------------------------

-- | Chart Element

------------------------------------------------------------------------------

module Soostone.Graphing.Chart.Element where

import Language.Javascript.JMacro

import Soostone.Graphing.D3
import Soostone.Graphing.Chart.Base
import Soostone.Graphing.Theme

------------------------------------------------------------------------------

-- | Wraps a Graph in an SVG group.

group :: Chart a JExpr
group = append "g"

-- | Wraps a Graph in an SVG rect.

rect :: Chart a JExpr
rect = append "rect" `with` defaultSize

------------------------------------------------------------------------------
