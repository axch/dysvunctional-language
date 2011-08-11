module FOL.Language.Pretty
    ( Pretty
    , pp
    , pprint
    , sepMap
    , ppList
    , ppForm
    , module Text.PrettyPrint
    )
    where

import Text.PrettyPrint

class Pretty a where
    pp :: a -> Doc

pprint :: Pretty a => a -> String
pprint = render . pp

sepMap :: (a -> Doc) -> [a] -> Doc
sepMap f = sep . map f

ppList :: [Doc] -> Doc
ppList = parens . sep

ppForm :: Pretty a => String -> [a] -> Doc
ppForm name xs = ppList $ text name : map pp xs
