{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main (main) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

class CPrint a where
    cprint :: a -> Ivory eff ()

instance CPrint Uint32 where
    cprint i = call_ printf_u32 "%u\n" i

instance CPrint IChar where
    cprint c = call_ printf_c "%c\n" c

instance CPrint IString where
    cprint s = call_ printf_s "%s\n" s

printf_u32 :: Def ('[IString, Uint32] ':-> ())
printf_u32 = importProc "printf" "stdio.h"

printf_c :: Def ('[IString, IChar] ':-> ())
printf_c = importProc "printf" "stdio.h"

printf_s :: Def ('[IString, IString] ':-> ())
printf_s = importProc "printf" "stdio.h"

cmain :: Def ('[] ':-> ())
cmain = proc "main" $ body $ do
    cprint (1 :: Uint32)
    cprint (char 'a')
    cprint ("hello" :: IString)
    retVoid

hello :: Module
hello = package "hello" $ do
    incl cmain
    incl printf_u32

main :: IO ()
main = C.compile [hello] []
