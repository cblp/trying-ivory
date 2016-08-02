{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main (main) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

printf2 :: IvoryType a => Def ('[IString, a] ':-> ())
printf2 = importProc "printf" "stdio.h"

class CPrint a where
    cprint :: a -> Ivory eff ()

instance CPrint Uint32 where
    cprint i = call_ printf2 "%u\n" i

instance CPrint IChar where
    cprint c = call_ printf2 "%c\n" c

instance CPrint IString where
    cprint s = call_ printf2 "%s\n" s

cmain :: Def ('[] ':-> ())
cmain = proc "main" $ body $ do
    cprint (1 :: Uint32)
    cprint (char 'a')
    cprint ("hello" :: IString)
    retVoid

hello :: Module
hello = package "hello" $ do
    incl cmain
    incl (printf2 :: Def ('[IString, IChar] ':-> ()))

main :: IO ()
main = C.compile [hello] []
