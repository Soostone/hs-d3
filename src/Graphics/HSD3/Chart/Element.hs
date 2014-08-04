------------------------------------------------------------------------------

-- | Chart Element

------------------------------------------------------------------------------

module Graphics.HSD3.Chart.Element where

import Language.Javascript.JMacro

import Graphics.HSD3.Chart.Base
import Graphics.HSD3.D3
import Graphics.HSD3.Theme

------------------------------------------------------------------------------

-- | Wraps a Graph in an SVG group.

group :: Chart a JExpr
group = append "g"

-- | Wraps a Graph in an SVG rect.

rect :: Chart a JExpr
rect = append "rect" `with` defaultSize

------------------------------------------------------------------------------
