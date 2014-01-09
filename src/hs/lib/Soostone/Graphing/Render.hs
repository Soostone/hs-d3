------------------------------------------------------------------------------

-- | This is the main interface, only file you shold need to import for REPL

------------------------------------------------------------------------------

module Soostone.Graphing.Render where

import Control.Lens
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text hiding (group, (<$>), width)

import Soostone.Graphing.Base

------------------------------------------------------------------------------

-- | Renders a GraphState as a string

renderGraph :: GraphState -> String
renderGraph =  renderText . view jstat

-- | Renders a `JExpr` as a string.

renderText :: JStat -> String
renderText = show . renderPretty 1.0 500 . renderJs . onload

------------------------------------------------------------------------------
