{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Animation where

import Fay.Text as T
import FFI

requestAnimationFrame :: Fay () -> Fay ()
requestAnimationFrame = ffi "window.requestAnimationFrame(%1)"
