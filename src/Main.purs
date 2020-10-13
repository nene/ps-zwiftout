module Main where

import Ast (Comment)

stripComments :: forall t. { comments :: Array Comment | t } -> { comments :: Array Comment | t }
stripComments interval = interval { comments = [] }
