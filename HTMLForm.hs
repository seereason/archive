module HTMLForm where

class HTMLForm a where
    toHtmlForm :: a -> Html

