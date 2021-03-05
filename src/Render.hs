module Render
  ( render,
    renderJSON,
  )
where

import CodeGen.Render
import RenderDefs

-- | Create a JSON representation of a program
renderJSON = asJSON . render

