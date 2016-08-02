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

-- | 'call_' a procedure with no arguments. Useful for variadic procedures.
-- call0_  :: IvoryCall_ ('[] ':-> ()) eff (Ivory eff ())
--         => Def        ('[] ':-> ()) ->  (Ivory eff ())
-- call0_ = call_

-- | 'call_' a procedure with 1 argument exactly. Useful for variadic procedures.
-- call1_  :: IvoryCall_ ('[a] ':-> ()) eff (a -> Ivory eff ())
--         => Def        ('[a] ':-> ()) ->  (a -> Ivory eff ())
-- call1_ = call_

-- | 'call_' a procedure with 2 arguments exactly. Useful for variadic procedures.
call2_  :: IvoryCall_ ('[a, b] ':-> ()) eff (a -> b -> Ivory eff ())
        => Def        ('[a, b] ':-> ()) ->  (a -> b -> Ivory eff ())
call2_ = call_

-- | 'call_' a procedure with 3 arguments exactly. Useful for variadic procedures.
-- call3_  :: IvoryCall_ ('[a, b, c] ':-> ()) eff (a -> b -> c -> Ivory eff ())
--         => Def        ('[a, b, c] ':-> ()) ->  (a -> b -> c -> Ivory eff ())
-- call3_ = call_

printf :: ProcType (args ':-> ()) => Def ((IString ': args) ':-> ())
printf = importProc "printf" "stdio.h"

class CPrint a where
    cprint :: a -> Ivory eff ()

instance CPrint Uint32 where
    cprint = call2_ printf "%u\n"

instance CPrint IChar where
    cprint = call2_ printf "%c\n"

instance CPrint IString where
    cprint = call2_ printf "%s\n"

cmain :: Def ('[] ':-> ())
cmain = proc "main" $ body $ do
    cprint (1 :: Uint32)
    cprint (char 'a')
    cprint ("hello" :: IString)

hello :: Module
hello = package "hello" $ do
    incl cmain
    incl (printf :: Def ('[IString] ':-> ()))

main :: IO ()
main = C.compile [hello] []
