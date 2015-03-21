--
-- I always find I have to import a lot of different Foreign.<blah> modules
-- in order to get all the functions I want. This module just packages those all up
-- into one module
--

module Foreign (
 module Foreign.Marshal.Alloc,
 module Foreign.Marshal.Array,
 module Foreign.Ptr,
 module Foreign.Storable,
 module Foreign.C.String,
 module Foreign.C.Types

) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
