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
import           Ivory.Language.Proc

call2_  :: IvoryCall_ ('[a, b] ':-> ()) eff (a -> b -> Ivory eff ())
        => Def ('[a, b] ':-> ()) -> (a -> b -> Ivory eff ())
call2_ = call_

printf :: ProcType (args ':-> ()) => Def ((IString ': args) ':-> ())
printf = importProc "printf" "stdio.h"

class CPrint a where
    cprint :: a -> Ivory eff ()

instance CPrint Uint32 where
    cprint i = call2_ printf "%u\n" i

instance CPrint IChar where
    cprint c = call2_ printf "%c\n" c

instance CPrint IString where
    cprint s = call2_ printf "%s\n" s

cmain :: Def ('[] ':-> ())
cmain = proc "main" $ body $ do
    cprint (1 :: Uint32)
    cprint (char 'a')
    cprint ("hello" :: IString)
    retVoid

hello :: Module
hello = package "hello" $ do
    incl cmain
    incl (printf :: Def ('[IString] ':-> ()))

main :: IO ()
main = C.compile [hello] []
